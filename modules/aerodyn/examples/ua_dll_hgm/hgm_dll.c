/* hgm_dll.c -- Reference implementation of OpenFAST's built-in UA_Mod=4
 * (HGM: Hansen-Gaunaa-Madsen 4-state Beddoes-Leishman variant) as a
 * standalone UA user-DLL (UA_Mod=9), ported term-for-term from
 * modules/aerodyn/src/UnsteadyAero.f90 so that a later acceptance test can
 * show DLL == built-in bit-for-bit (within double-precision RK4 integration
 * error). Every non-obvious formula below cites its Fortran source line;
 * see .superpowers/sdd/task-5-report.md for the full traceability table and
 * documented approximations.
 *
 * Plain C11. No external dependencies beyond libm.
 */
#include "ua_dll_api.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

/* ------------------------------------------------------------------ */
/* Constants (all cite their Fortran origin)                          */
/* ------------------------------------------------------------------ */
static const double PI_C      = 3.14159265358979323846;
static const double TWOPI_C   = 6.28318530717958647692;
static const double PIBY2_C   = 1.57079632679489661923;

/* UnsteadyAero.f90:63 -- floor on relative velocity so UA math doesn't blow up */
static const double UA_U_MIN = 0.01;
/* UnsteadyAero.f90:65 -- safety clamp on Tu*omega */
static const double MAX_TU_OMEGA = 1.5;
/* AirfoilInfo.f90:878 -- alpha range used to hunt for default Cd0 (UAMod /= HGMV360) */
static const double LIMIT_ALPHA_RANGE = 20.0 * 3.14159265358979323846 / 180.0;

/* AirfoilInfo.f90:901-909 -- CalculateUACoeffs "Set to default values:" block.
 * These are the DEFAULT values AirfoilInfo assigns to the UA_BL type when the
 * airfoil file specifies "DEFAULT" for each field (which the acceptance case
 * MUST do -- see task-5-report.md -- for the DLL and built-in to agree). */
static const double T_F0_DEFAULT = 3.00;  /* AirfoilInfo.f90:901 */
static const double T_P_DEFAULT  = 1.70;  /* AirfoilInfo.f90:903 */
static const double B1_DEFAULT   = 0.14;  /* AirfoilInfo.f90:905 */
static const double B2_DEFAULT   = 0.53;  /* AirfoilInfo.f90:906 */
static const double A1_DEFAULT   = 0.30;  /* AirfoilInfo.f90:908 */
static const double A2_DEFAULT   = 0.70;  /* AirfoilInfo.f90:909 */

/* UnsteadyAero_Registry.txt:39 -- ShedEffect default .True.; the ABI has no
 * way to pass this through, so it is hardcoded true (the only value the
 * acceptance case can use since UA_DLL_InitInput carries no such flag). */
static const int SHED_EFFECT = 1;

/* ------------------------------------------------------------------ */
/* Small numeric helpers                                              */
/* ------------------------------------------------------------------ */

/* NWTC_Num.f90 MPi2Pi_R8: wrap angle to (-pi, pi] */
static double wrap_pi(double angle) {
    double a = fmod(angle, TWOPI_C);
    if (a < 0.0) a += TWOPI_C;      /* now in [0, 2pi) -- MODULO semantics */
    if (a > PI_C) a -= TWOPI_C;
    return a;
}

/* NWTC_Num.f90:328-362 AddOrSub2Pi_R8: nudge new_angle by multiples of 2*pi
 * until it is within pi of old_angle. Mutates *new_angle in place, exactly
 * mirroring the Fortran INTENT(INOUT) NewAngle argument. */
static void add_or_sub_2pi(double old_angle, double *new_angle) {
    double del = old_angle - *new_angle;
    int n = (int)(del / TWOPI_C);
    *new_angle += (double)n * TWOPI_C;
    del = old_angle - *new_angle;
    for (int i = 0; i < 10 && fabs(del) > PI_C && fabs(old_angle - *new_angle) > 1e-12; i++) {
        *new_angle += (del >= 0.0 ? TWOPI_C : -TWOPI_C);
        del = old_angle - *new_angle;
    }
}

/* Clip + linear interpolation. AirfoilInfo uses cubic-spline interpolation
 * (CubicSplineInterpM) over the polar tables; this DLL uses simple clip+lerp
 * instead. Documented fidelity gap -- see task-5-report.md. */
static double interp1(const double *x, const double *y, int32_t n, double xi) {
    if (n <= 0) return 0.0;
    if (n == 1 || xi <= x[0]) return y[0];
    if (xi >= x[n - 1]) return y[n - 1];
    /* binary search */
    int32_t lo = 0, hi = n - 1;
    while (hi - lo > 1) {
        int32_t mid = (lo + hi) / 2;
        if (x[mid] <= xi) lo = mid; else hi = mid;
    }
    double t = (xi - x[lo]) / (x[hi] - x[lo]);
    return y[lo] + t * (y[hi] - y[lo]);
}

static double clampd(double v, double lo, double hi) {
    if (v < lo) return lo;
    if (v > hi) return hi;
    return v;
}

/* ------------------------------------------------------------------ */
/* Per-polar precomputed tables                                       */
/* ------------------------------------------------------------------ */
typedef struct {
    int32_t n_alpha;
    double *alpha, *Cl, *Cd, *Cm; /* deep copies; Cm may be NULL (no Cm column) */
    double  alpha0;    /* == Fortran BL_p%alpha0; supplied verbatim by the ABI
                           (UA_Dll.f90:225 sets it from tab%UA_BL%alpha0, which
                           is already the DEFAULT-derived value) */
    double  Cl_alpha;  /* == Fortran BL_p%c_lalpha; UA_Dll.f90:226, same story */
    double  Cd0;       /* AirfoilInfo.f90:929-937,972: min(Cd) over |alpha|<=20deg;
                           derived here from the polar's own Cd array (ABI gives
                           us alpha/Cd directly, so this is exact, not hardcoded) */
    double *f_st;       /* [n_alpha] separation function on the native alpha grid,
                            AirfoilInfo.f90 ComputeUASeparationFunction_onCl:1245-1263 */
    double *cl_fs;       /* [n_alpha] fully-separated Cl on the native alpha grid, same routine */
} Polar;

/* AirfoilInfo.f90 ComputeUASeparationFunction_onCl, lines 1245-1263 (first
 * pass only -- the "Ensuring everything is in harmony" second pass,
 * lines 1279-1292, is an algebraic no-op away from clipped boundary rows and
 * is not reproduced here; see task-5-report.md). This is the exact same
 * Kirchhoff-inversion formula used by the Oye reference DLL's fs_st/cl_fs.
 * Returns 0 on success, -1 on allocation failure. */
static int build_separation_tables(Polar *p) {
    p->f_st  = (double *)malloc(sizeof(double) * (size_t)p->n_alpha);
    p->cl_fs = (double *)malloc(sizeof(double) * (size_t)p->n_alpha);
    if (!p->f_st || !p->cl_fs) return -1; /* partial allocs freed by free_ctx */
    for (int32_t i = 0; i < p->n_alpha; i++) {
        double a = p->alpha[i];
        double cl = p->Cl[i];
        double f_st, fs;
        if (fabs(p->Cl_alpha) < 1e-12) {
            /* AirfoilInfo.f90:1236-1240: c_lalpha==0 (cylinder-like polar) */
            f_st = 0.0;
            fs   = cl;
        } else if (fabs(a - p->alpha0) < 1e-10) {
            f_st = 1.0;                 /* Eq. 59 */
            fs   = cl * 0.5;            /* Eq. 61 */
        } else {
            double cl_ratio = cl / (p->Cl_alpha * (a - p->alpha0));
            if (cl_ratio < 0.0) cl_ratio = 0.0;
            f_st = (2.0 * sqrt(cl_ratio) - 1.0);
            f_st = f_st * f_st;
            if (f_st < 1.0) {
                if (f_st < 0.0) f_st = 0.0;
                fs = (cl - p->Cl_alpha * (a - p->alpha0) * f_st) / (1.0 - f_st); /* Eq. 61 */
            } else {
                f_st = 1.0;
                fs = cl * 0.5;
            }
        }
        p->f_st[i]  = f_st;
        p->cl_fs[i] = fs;
    }
    return 0;
}

static double fst_query(const Polar *p, double alpha) {
    double f = interp1(p->alpha, p->f_st, p->n_alpha, alpha);
    return clampd(f, 0.0, 1.0); /* AirfoilInfo.f90:1818 */
}
static double clfs_query(const Polar *p, double alpha) {
    return interp1(p->alpha, p->cl_fs, p->n_alpha, alpha);
}
static double cd_query(const Polar *p, double alpha) {
    return interp1(p->alpha, p->Cd, p->n_alpha, alpha);
}
static double cm_query(const Polar *p, double alpha) {
    if (!p->Cm) return 0.0; /* AirfoilInfo.f90:1754-1758: no Cm column -> 0 */
    return interp1(p->alpha, p->Cm, p->n_alpha, alpha);
}

/* AirfoilInfo.f90:929-937,972 default Cd0: min(Cd) over |alpha| <= 20 deg */
static double compute_default_cd0(const Polar *p) {
    int32_t iMin = -1;
    double best = 0.0;
    for (int32_t i = 0; i < p->n_alpha; i++) {
        if (fabs(p->alpha[i]) <= LIMIT_ALPHA_RANGE) {
            if (iMin < 0 || p->Cd[i] < best) { best = p->Cd[i]; iMin = i; }
        }
    }
    if (iMin < 0) {
        /* fallback: no points within +/-20deg -- use global min */
        for (int32_t i = 0; i < p->n_alpha; i++) {
            if (iMin < 0 || p->Cd[i] < best) { best = p->Cd[i]; iMin = i; }
        }
    }
    return (iMin >= 0) ? best : 0.0;
}

/* ------------------------------------------------------------------ */
/* Per-element state                                                  */
/* ------------------------------------------------------------------ */
typedef struct {
    double  x[4];       /* x1,x2: downwash lag; x3: Clp' (lift-lag); x4: f'' separation-fn lag */
    double  chord;
    int32_t polar_id;
    int     initialized; /* lazy steady-state init on first touch, mirrors
                             OtherState%FirstPass -- see task-5-report.md for why
                             this can't happen inside ua_dll_init (no elem inputs there) */
} Elem;

typedef struct {
    int32_t n_elem;
    double  dt;
    double  d34_frac;  /* UA_DllInitInput.d_34_to_ac: 3/4-chord-to-AC distance, chords */
    Elem   *e;
    Polar  *polars;
    int32_t n_polars;
} Ctx;

/* ------------------------------------------------------------------ */
/* Kinematics -- UnsteadyAero.f90:2915-2940                            */
/* ------------------------------------------------------------------ */

/* UnsteadyAero.f90 Get_Alpha34, lines 2914-2923 */
static double get_alpha34(double vx, double vy, double omega, double d34_dist) {
    double vx34 = vx + omega * d34_dist;
    return atan2(vx34, vy);
}

/* UnsteadyAero.f90 Get_Tu, lines 2933-2940. NOTE: uses max(U,UA_u_min), not
 * max(|U|,UA_u_min) -- ported exactly, including the asymmetric behavior for
 * strongly negative U. */
static double get_tu(double U, double chord) {
    double tu = chord / (2.0 * fmax(U, UA_U_MIN));
    tu = fmin(tu, 50.0);
    tu = fmax(tu, 0.001);
    return tu;
}

/* UnsteadyAero.f90 UA_fixInputs, lines 4308-4328: wrap alpha to (-pi,pi], and
 * if |U| is too small, floor it (sign-preserving) and rebuild v_ac from
 * (alpha,U) so downstream math doesn't blow up. */
typedef struct { double U, alpha, vx, vy; } FixedInput;
static FixedInput fix_inputs(double U, double alpha, double vx, double vy) {
    FixedInput f;
    f.alpha = wrap_pi(alpha);
    f.U = U;
    f.vx = vx;
    f.vy = vy;
    if (fabs(f.U) < UA_U_MIN) {
        f.U = copysign(UA_U_MIN, f.U);
        f.vx = sin(f.alpha) * f.U;
        f.vy = cos(f.alpha) * f.U;
    }
    return f;
}

/* ------------------------------------------------------------------ */
/* HGM state derivative -- UnsteadyAero.f90 UA_CalcContStateDeriv, the
 * UA_HGM branch (lines 2658-2815), and Get_HGM_constants/Get_alphaF
 * (lines 2817-2912).                                                  */
/* ------------------------------------------------------------------ */
static void hgm_deriv(const double x[4], const Polar *pol, double chord, double d34_frac,
                       const UA_DllElemInput *u_raw, double dxdt[4]) {
    FixedInput fi = fix_inputs(u_raw->U, u_raw->alpha34, u_raw->v_ac_x, u_raw->v_ac_y);
    double omega = u_raw->omega;

    double Tu = get_tu(fi.U, chord);                                  /* Get_Tu */
    double alpha_34 = get_alpha34(fi.vx, fi.vy, omega, d34_frac * chord); /* Get_Alpha34 */
    if (SHED_EFFECT) alpha_34 = wrap_pi(alpha_34);                     /* Get_HGM_constants:2842 */

    double alphaE = alpha_34 * (1.0 - A1_DEFAULT - A2_DEFAULT) + x[0] + x[1]; /* Eq. 12, :2844 */

    double T_f0 = T_F0_DEFAULT * Tu;   /* UnsteadyAero.f90:2722 */
    double T_p  = T_P_DEFAULT  * Tu;   /* UnsteadyAero.f90:2723 */
    double TuOmega = clampd(Tu * omega, -MAX_TU_OMEGA, MAX_TU_OMEGA);  /* :2725-2726 */

    /* Get_alphaF, UA_HGM branch: alphaF = x3/c_lalpha + alpha0 -- Eq. 15, :2877 */
    double alphaF = x[2] / pol->Cl_alpha + pol->alpha0;
    double f_st_alphaF = fst_query(pol, alphaF);

    double x4 = clampd(x[3], 0.0, 1.0); /* :2744 */

    /* Downwash lag states (ShedEffect always true here) -- :2746-2755 */
    double a34_for_x1 = alpha_34;
    if (fabs(A1_DEFAULT) > 1e-12) add_or_sub_2pi(x[0] / A1_DEFAULT, &a34_for_x1); /* :2747 */
    dxdt[0] = -1.0 / Tu * B1_DEFAULT * x[0] + B1_DEFAULT * A1_DEFAULT / Tu * a34_for_x1; /* Eq. 8 */

    double a34_for_x2 = a34_for_x1; /* Fortran mutates the same local alpha_34 cumulatively */
    if (fabs(A2_DEFAULT) > 1e-12) add_or_sub_2pi(x[1] / A2_DEFAULT, &a34_for_x2); /* :2750 */
    dxdt[1] = -1.0 / Tu * B2_DEFAULT * x[1] + B2_DEFAULT * A2_DEFAULT / Tu * a34_for_x2; /* Eq. 9 */

    /* Cl-lag and separation-fn-lag states, UA_HGM branch -- :2757-2762 */
    double alphaE3 = alphaE;
    add_or_sub_2pi(pol->alpha0, &alphaE3); /* :2758 */
    double Clp = pol->Cl_alpha * (alphaE3 - pol->alpha0) + PI_C * TuOmega; /* Eq. 13 */
    dxdt[2] = (Clp - x[2]) / T_p;              /* Eq. 10 */
    dxdt[3] = (f_st_alphaF - x4) / T_f0;       /* Eq. 11 */
}

/* ------------------------------------------------------------------ */
/* Steady-state init -- UnsteadyAero.f90 HGM_Steady, lines 2559-2656.
 * x4 IS set at steady state: the assignment at line 2653
 * (x%x(4) = AFI_interp%f_st) runs unconditionally after the per-model
 * branch, with AFI_interp evaluated at alphaF = alphaE = alpha_34
 * (lines 2617-2619). Only the HGMV-branch recomputation at 2643-2645 is
 * commented out in the Fortran.                                           */
/* ------------------------------------------------------------------ */
static void hgm_steady_init(double x[4], const Polar *pol, double chord, double d34_frac,
                             const UA_DllElemInput *u_raw) {
    FixedInput fi = fix_inputs(u_raw->U, u_raw->alpha34, u_raw->v_ac_x, u_raw->v_ac_y);
    double omega = u_raw->omega;

    double alpha_34 = get_alpha34(fi.vx, fi.vy, omega, d34_frac * chord);
    if (SHED_EFFECT) alpha_34 = wrap_pi(alpha_34);

    x[0] = SHED_EFFECT ? A1_DEFAULT * alpha_34 : 0.0; /* :2613 */
    x[1] = SHED_EFFECT ? A2_DEFAULT * alpha_34 : 0.0; /* :2614 */

    double alphaE = alpha_34; /* :2617, after substituting x1,x2 initializations */
    x[2] = pol->Cl_alpha * (alphaE - pol->alpha0);    /* :2626 */
    x[3] = fst_query(pol, alpha_34);                  /* :2653, AFI_interp at alphaF==alpha_34 (:2618-2619) */
}

/* ------------------------------------------------------------------ */
/* Output -- UnsteadyAero.f90 UA_CalcOutput, UA_HGM branch, lines
 * 3658-3681 (with shared setup at 3594-3626 and CosAlpha/SinAlpha at
 * 3519-3526).                                                          */
/* ------------------------------------------------------------------ */
static void hgm_output(const double x[4], const Polar *pol, double chord, double d34_frac,
                        const UA_DllElemInput *u_raw, UA_DllElemOutput *y) {
    FixedInput fi = fix_inputs(u_raw->U, u_raw->alpha34, u_raw->v_ac_x, u_raw->v_ac_y);
    double omega = u_raw->omega;

    double Tu = get_tu(fi.U, chord);
    double alpha_34 = get_alpha34(fi.vx, fi.vy, omega, d34_frac * chord);
    if (SHED_EFFECT) alpha_34 = wrap_pi(alpha_34);
    double alphaE = alpha_34 * (1.0 - A1_DEFAULT - A2_DEFAULT) + x[0] + x[1]; /* Eq. 12 */
    double TuOmega = clampd(Tu * omega, -MAX_TU_OMEGA, MAX_TU_OMEGA);

    double x4 = clampd(x[3], 0.0, 1.0);

    double fs_aE = fst_query(pol, alphaE);   /* AFI_interpE%f_st */
    double cl_fs = clfs_query(pol, alphaE);  /* AFI_interpE%FullySeparate */
    double Cd_aE = cd_query(pol, alphaE);
    double Cm_aE = cm_query(pol, alphaE);

    double alphaE2 = alphaE;
    add_or_sub_2pi(pol->alpha0, &alphaE2);                 /* :3660 */
    double cl_fa = (alphaE2 - pol->alpha0) * pol->Cl_alpha; /* :3661 */

    double delta_c_df_pp = 0.5 * (sqrt(fs_aE) - sqrt(x4)) - 0.25 * (fs_aE - x4); /* Eq. 20, :3663 */
    double cl_circ = x4 * cl_fa + (1.0 - x4) * cl_fs; /* Eq. 19, :3666 */
    double Cl = cl_circ + PI_C * TuOmega;             /* Eq. 16, :3667 */
    double cd_tors = cl_circ * TuOmega;               /* :3669 */

    add_or_sub_2pi(alpha_34, &alphaE2);               /* :3670, mutates the already-wrapped alphaE2 */
    double Cd = Cd_aE + (alpha_34 - alphaE2) * cl_circ
              + (Cd_aE - pol->Cd0) * delta_c_df_pp + cd_tors; /* Eq. 17, :3671 */

    double Cm = Cm_aE - PIBY2_C * TuOmega; /* Eq. 18, :3676 (delta_c_mf_primeprime == 0 always, :3491) */

    double CosAlpha = cos(fi.alpha); /* :3525, u%alpha == AC-relative angle, our alpha34 field per UA_Dll.f90:318 */
    double SinAlpha = sin(fi.alpha); /* :3526 */

    y->Cl = Cl;
    y->Cd = Cd;
    y->Cm = Cm;
    y->Cn = Cl * CosAlpha + Cd * SinAlpha; /* :3679 */
    y->Cc = Cl * SinAlpha - Cd * CosAlpha; /* :3680 */
}

/* Frees a (possibly partially constructed) context. Safe on NULL members
 * because everything is calloc-zeroed before any malloc can fail. */
static void free_ctx(Ctx *c) {
    if (!c) return;
    if (c->polars) {
        for (int32_t k = 0; k < c->n_polars; k++) {
            Polar *p = &c->polars[k];
            free(p->alpha); free(p->Cl); free(p->Cd); free(p->Cm);
            free(p->f_st); free(p->cl_fs);
        }
        free(c->polars);
    }
    free(c->e);
    free(c);
}

/* ------------------------------------------------------------------ */
/* ABI entry points                                                    */
/* ------------------------------------------------------------------ */

int32_t ua_dll_getinfo(UA_DllInfo *info, char *msg, int32_t msg_len) {
    if (info->abi_version != UA_DLL_ABI_VERSION) {
        snprintf(msg, (size_t)msg_len, "ABI %d != %d", info->abi_version, UA_DLL_ABI_VERSION);
        return -1;
    }
    if (info->struct_size < (int32_t)sizeof(UA_DllInfo)) {
        snprintf(msg, (size_t)msg_len, "ua_dll_getinfo: struct_size %d < %d",
                 info->struct_size, (int32_t)sizeof(UA_DllInfo));
        return -1;
    }
    snprintf(info->model_name, sizeof info->model_name, "Reference HGM (C)");
    info->n_states_per_elem = 4;
    info->caps    = UA_DLL_CAP_PACK;
    info->dt_min  = 0.0;
    info->dt_max  = 0.0;
    (void)msg; (void)msg_len;
    return UA_DLL_OK;
}

int32_t ua_dll_init(const UA_DllInitInput *init, void **ctx, char *msg, int32_t msg_len) {
    if (init->abi_version != UA_DLL_ABI_VERSION) {
        snprintf(msg, (size_t)msg_len, "ua_dll_init: ABI %d != %d", init->abi_version, UA_DLL_ABI_VERSION);
        return -1;
    }
    if (init->struct_size < (int32_t)sizeof(UA_DllInitInput)) {
        snprintf(msg, (size_t)msg_len, "ua_dll_init: struct_size %d < %d",
                 init->struct_size, (int32_t)sizeof(UA_DllInitInput));
        return -1;
    }

    Ctx *c = (Ctx *)calloc(1, sizeof(Ctx));
    if (!c) { snprintf(msg, (size_t)msg_len, "ua_dll_init: out of memory"); return -1; }

    c->dt       = init->dt;
    c->d34_frac = init->d_34_to_ac;
    c->n_polars = init->n_polars;
    c->n_elem   = init->n_blades * init->n_nodes_per_blade;

    /* Deep-copy polars: init->polars and everything it points to is not
     * guaranteed to persist past this call (per ua_dll_api.h contract).
     * Every allocation is checked; on failure, free_ctx releases whatever
     * was built so far (calloc-zeroed members make partial frees safe). */
    c->polars = (Polar *)calloc((size_t)(c->n_polars > 0 ? c->n_polars : 1), sizeof(Polar));
    if (!c->polars) {
        free_ctx(c);
        snprintf(msg, (size_t)msg_len, "ua_dll_init: out of memory (polars)");
        return -1;
    }
    for (int32_t k = 0; k < c->n_polars; k++) {
        const UA_DllPolar *src = &init->polars[k];
        Polar *p = &c->polars[k];
        p->n_alpha  = src->n_alpha;
        p->alpha    = (double *)malloc(sizeof(double) * (size_t)p->n_alpha);
        p->Cl       = (double *)malloc(sizeof(double) * (size_t)p->n_alpha);
        p->Cd       = (double *)malloc(sizeof(double) * (size_t)p->n_alpha);
        p->Cm       = src->Cm ? (double *)malloc(sizeof(double) * (size_t)p->n_alpha) : NULL;
        if (!p->alpha || !p->Cl || !p->Cd || (src->Cm && !p->Cm)) {
            free_ctx(c);
            snprintf(msg, (size_t)msg_len, "ua_dll_init: out of memory (polar %d tables)", k);
            return -1;
        }
        memcpy(p->alpha, src->alpha, sizeof(double) * (size_t)p->n_alpha);
        memcpy(p->Cl,    src->Cl,    sizeof(double) * (size_t)p->n_alpha);
        memcpy(p->Cd,    src->Cd,    sizeof(double) * (size_t)p->n_alpha);
        if (src->Cm) memcpy(p->Cm, src->Cm, sizeof(double) * (size_t)p->n_alpha);
        p->alpha0   = src->alpha0;   /* UA_Dll.f90:225, exact match to BL_p%alpha0 */
        p->Cl_alpha = src->Cl_alpha; /* UA_Dll.f90:226, exact match to BL_p%c_lalpha */
        p->Cd0      = compute_default_cd0(p);
        if (build_separation_tables(p) != 0) {
            free_ctx(c);
            snprintf(msg, (size_t)msg_len, "ua_dll_init: out of memory (polar %d separation tables)", k);
            return -1;
        }
    }

    c->e = (Elem *)calloc((size_t)(c->n_elem > 0 ? c->n_elem : 1), sizeof(Elem));
    if (!c->e) {
        free_ctx(c);
        snprintf(msg, (size_t)msg_len, "ua_dll_init: out of memory (elements)");
        return -1;
    }
    for (int32_t i = 0; i < c->n_elem; i++) {
        c->e[i].chord       = init->chord[i];
        c->e[i].polar_id    = init->polar_id[i];
        c->e[i].initialized = 0;
        c->e[i].x[0] = c->e[i].x[1] = c->e[i].x[2] = c->e[i].x[3] = 0.0;
    }

    *ctx = c;
    (void)msg; (void)msg_len;
    return UA_DLL_OK;
}

int32_t ua_dll_update(void *ctx, double t, int64_t step,
                       const UA_DllElemInput *u_t, const UA_DllElemInput *u_tp1,
                       int32_t n_elem, char *msg, int32_t msg_len) {
    Ctx *c = (Ctx *)ctx;
    (void)t; (void)step; (void)msg; (void)msg_len;
    if (n_elem != c->n_elem) {
        snprintf(msg, (size_t)msg_len, "ua_dll_update: n_elem mismatch (%d != %d)", n_elem, c->n_elem);
        return -1;
    }

    for (int32_t i = 0; i < n_elem; i++) {
        Elem *e = &c->e[i];
        const Polar *pol = &c->polars[e->polar_id];

        if (!e->initialized) {
            /* Lazy steady-state init on first touch, using the "t" endpoint
             * input -- mirrors UA_UpdateStates:2389-2390 (u_interp at t). */
            hgm_steady_init(e->x, pol, e->chord, c->d34_frac, &u_t[i]);
            e->initialized = 1;
        }

        UA_DllElemInput u_half;
        u_half.U        = 0.5 * (u_t[i].U        + u_tp1[i].U);
        u_half.alpha34  = 0.5 * (u_t[i].alpha34   + u_tp1[i].alpha34);
        u_half.Re       = 0.5 * (u_t[i].Re        + u_tp1[i].Re);
        u_half.UserProp = 0.5 * (u_t[i].UserProp  + u_tp1[i].UserProp);
        u_half.v_ac_x   = 0.5 * (u_t[i].v_ac_x    + u_tp1[i].v_ac_x);
        u_half.v_ac_y   = 0.5 * (u_t[i].v_ac_y    + u_tp1[i].v_ac_y);
        u_half.omega    = 0.5 * (u_t[i].omega     + u_tp1[i].omega);

        /* RK4, UnsteadyAero.f90 UA_RK4:2960-3051, with stage inputs linearly
         * interpolated between the two endpoints the ABI provides (exactly
         * equivalent to the built-in's UA_Input_ExtrapInterp when only 2
         * input points are available, which is the standard driver case). */
        double x0[4], xt[4], k1[4], k2[4], k3[4], k4[4];
        memcpy(x0, e->x, sizeof(x0));

        hgm_deriv(x0, pol, e->chord, c->d34_frac, &u_t[i], k1);
        for (int j = 0; j < 4; j++) { k1[j] *= c->dt; xt[j] = x0[j] + 0.5 * k1[j]; }

        hgm_deriv(xt, pol, e->chord, c->d34_frac, &u_half, k2);
        for (int j = 0; j < 4; j++) { k2[j] *= c->dt; xt[j] = x0[j] + 0.5 * k2[j]; }

        hgm_deriv(xt, pol, e->chord, c->d34_frac, &u_half, k3);
        for (int j = 0; j < 4; j++) { k3[j] *= c->dt; xt[j] = x0[j] + k3[j]; }

        hgm_deriv(xt, pol, e->chord, c->d34_frac, &u_tp1[i], k4);
        for (int j = 0; j < 4; j++) { k4[j] *= c->dt; }

        for (int j = 0; j < 4; j++)
            e->x[j] = x0[j] + (k1[j] + 2.0 * k2[j] + 2.0 * k3[j] + k4[j]) / 6.0;

        e->x[3] = clampd(e->x[3], 0.0, 1.0); /* UnsteadyAero.f90:2427 */
    }
    return UA_DLL_OK;
}

int32_t ua_dll_output(void *ctx, double t, const UA_DllElemInput *u, int32_t n_elem,
                       UA_DllElemOutput *y, char *msg, int32_t msg_len) {
    Ctx *c = (Ctx *)ctx;
    (void)t;
    if (n_elem != c->n_elem) {
        snprintf(msg, (size_t)msg_len, "ua_dll_output: n_elem mismatch (%d != %d)", n_elem, c->n_elem);
        return -1;
    }
    for (int32_t i = 0; i < n_elem; i++) {
        const Elem *e = &c->e[i];
        const Polar *pol = &c->polars[e->polar_id];
        if (!e->initialized) {
            /* FirstPass path, mirrors UA_CalcOutput:3596-3601: the built-in
             * runs HGM_Steady on a LOCAL copy (x_in) that is discarded after
             * the call -- OtherState%FirstPass is not cleared and x%element
             * is not written. So: compute steady states into a stack array
             * for this call only; NO writes to ctx (ua_dll_api.h: output
             * MUST NOT modify ctx state). Repeated output calls before the
             * first update therefore recompute steady state from each call's
             * own inputs, exactly like the built-in. */
            double x_local[4];
            hgm_steady_init(x_local, pol, e->chord, c->d34_frac, &u[i]);
            hgm_output(x_local, pol, e->chord, c->d34_frac, &u[i], &y[i]);
        } else {
            hgm_output(e->x, pol, e->chord, c->d34_frac, &u[i], &y[i]);
        }
    }
    (void)msg; (void)msg_len;
    return UA_DLL_OK;
}

int32_t ua_dll_pack(void *ctx, unsigned char *buf, int64_t *n_bytes, char *msg, int32_t msg_len) {
    Ctx *c = (Ctx *)ctx;
    (void)msg; (void)msg_len;
    int64_t needed = (int64_t)sizeof(int32_t) + (int64_t)c->n_elem * 4 * (int64_t)sizeof(double);
    if (buf == NULL) {
        *n_bytes = needed;
        return UA_DLL_OK;
    }
    if (*n_bytes < needed) {
        snprintf(msg, (size_t)msg_len, "ua_dll_pack: buffer too small (%lld < %lld)",
                 (long long)*n_bytes, (long long)needed);
        return -1;
    }
    unsigned char *p = buf;
    int32_t n = c->n_elem;
    memcpy(p, &n, sizeof(int32_t)); p += sizeof(int32_t);
    for (int32_t i = 0; i < c->n_elem; i++) {
        memcpy(p, c->e[i].x, 4 * sizeof(double));
        p += 4 * sizeof(double);
    }
    *n_bytes = needed;
    return UA_DLL_OK;
}

int32_t ua_dll_unpack(void *ctx, const unsigned char *buf, int64_t n_bytes, char *msg, int32_t msg_len) {
    Ctx *c = (Ctx *)ctx;
    int64_t expected = (int64_t)sizeof(int32_t) + (int64_t)c->n_elem * 4 * (int64_t)sizeof(double);
    if (n_bytes != expected) {
        snprintf(msg, (size_t)msg_len, "ua_dll_unpack: size mismatch (%lld != %lld)",
                 (long long)n_bytes, (long long)expected);
        return -1;
    }
    const unsigned char *p = buf;
    int32_t n;
    memcpy(&n, p, sizeof(int32_t)); p += sizeof(int32_t);
    if (n != c->n_elem) {
        snprintf(msg, (size_t)msg_len, "ua_dll_unpack: n_elem mismatch (%d != %d)", n, c->n_elem);
        return -1;
    }
    for (int32_t i = 0; i < c->n_elem; i++) {
        memcpy(c->e[i].x, p, 4 * sizeof(double));
        p += 4 * sizeof(double);
        c->e[i].initialized = 1; /* restart implies steady-state init already happened */
    }
    return UA_DLL_OK;
}

int32_t ua_dll_end(void *ctx, char *msg, int32_t msg_len) {
    (void)msg; (void)msg_len;
    free_ctx((Ctx *)ctx);
    return UA_DLL_OK;
}
