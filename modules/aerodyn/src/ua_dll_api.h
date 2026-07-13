/* ua_dll_api.h — OpenFAST UnsteadyAero user-DLL ABI (UA_Mod = 9)
 * ABI stability contract: structs are extended only by appending fields;
 * callers/DLLs must check abi_version and struct_size before use.       */
#ifndef UA_DLL_API_H
#define UA_DLL_API_H
#include <stdint.h>

#define UA_DLL_ABI_VERSION 1

/* return codes: 0 = ok, >0 = warning (sim continues), <0 = fatal */
#define UA_DLL_OK    0

/* capability bits reported by ua_dll_getinfo */
#define UA_DLL_CAP_PACK   (1u << 0)  /* pack/unpack implemented (required for restart) */

#ifdef __cplusplus
extern "C" {
#endif

typedef struct UA_DllInfo {
    int32_t abi_version;      /* [in] set by caller to UA_DLL_ABI_VERSION; DLL must verify */
    int32_t struct_size;      /* [in] sizeof(UA_DllInfo) as compiled by caller            */
    char    model_name[64];   /* [out] human-readable model name                           */
    int32_t n_states_per_elem;/* [out] informational; -1 if unknown/variable               */
    uint32_t caps;            /* [out] UA_DLL_CAP_* bits                                   */
    double  dt_min;           /* [out] smallest dt the model supports; 0 = no limit        */
    double  dt_max;           /* [out] largest  dt the model supports; 0 = no limit        */
} UA_DllInfo;

typedef struct UA_DllPolar {   /* one airfoil table (one Re/UserProp table)               */
    int32_t abi_version, struct_size;
    int32_t n_alpha;
    const double *alpha;       /* [n_alpha] rad, ascending, typically -pi..pi              */
    const double *Cl, *Cd, *Cm;/* [n_alpha] each; Cm may be NULL if unavailable            */
    double  alpha0;            /* zero-lift AoA, rad     */
    double  Cl_alpha;          /* lift slope, 1/rad      */
    double  Re;                /* table Reynolds number  */
    double  UserProp;          /* table user property    */
} UA_DllPolar;

typedef struct UA_DllInitInput {
    int32_t abi_version, struct_size;
    double  dt;                /* glue/UA time step, s                                     */
    double  a_s;               /* speed of sound, m/s                                      */
    double  d_34_to_ac;        /* 3/4-chord to AC distance, chords                         */
    int32_t n_blades;
    int32_t n_nodes_per_blade; /* elements are indexed elem = iB*n_nodes_per_blade + iN    */
    const double *chord;       /* [n_elem] chord per element, m                            */
    const int32_t *polar_id;   /* [n_elem] 0-based index into polars                       */
    int32_t n_polars;
    const UA_DllPolar *polars; /* [n_polars]                                               */
    const char *param_str;     /* UADLLParamFile string from the input file (may be "");
                                  intended use: path to the DLL's own config/weights file  */
    int32_t param_len;         /* strlen(param_str)                                        */
} UA_DllInitInput;

typedef struct UA_DllElemInput {
    double U;        /* relative velocity magnitude at AC, m/s        */
    double alpha34;  /* AoA at 3/4 chord, rad                          */
    double Re;       /* Reynolds number, -                             */
    double UserProp; /* table-interpolation property                   */
    double v_ac_x;   /* AC-relative velocity components, m/s           */
    double v_ac_y;
    double omega;    /* section pitch/twist rate, rad/s                */
} UA_DllElemInput;

typedef struct UA_DllElemOutput {
    double Cn, Cc, Cl, Cd, Cm;
} UA_DllElemOutput;

/* Entry points (fixed names; resolved by dlsym/GetProcAddress).
 * Every call: msg is a caller-owned buffer of msg_len bytes for a
 * null-terminated diagnostic on warning/fatal returns.                  */

int32_t ua_dll_getinfo(UA_DllInfo *info, char *msg, int32_t msg_len);

int32_t ua_dll_init(const UA_DllInitInput *init, void **ctx,
                    char *msg, int32_t msg_len);

/* Advance ALL element states from t to t+dt. u_t and u_tp1 are the inputs
 * at the interval endpoints (n_elem entries each; OpenFAST provides both
 * so models may use any quadrature over the interval).                   */
int32_t ua_dll_update(void *ctx, double t, int64_t step,
                      const UA_DllElemInput *u_t,
                      const UA_DllElemInput *u_tp1,
                      int32_t n_elem, char *msg, int32_t msg_len);

/* Outputs at time t for current states. MUST NOT modify ctx state.      */
int32_t ua_dll_output(void *ctx, double t,
                      const UA_DllElemInput *u, int32_t n_elem,
                      UA_DllElemOutput *y, char *msg, int32_t msg_len);

/* Serialize state. Call with buf=NULL to query required size in *n_bytes;
 * second call with an allocated buf writes it.                           */
int32_t ua_dll_pack(void *ctx, unsigned char *buf, int64_t *n_bytes,
                    char *msg, int32_t msg_len);

/* Restore state (ctx already created by ua_dll_init on the restarted run) */
int32_t ua_dll_unpack(void *ctx, const unsigned char *buf, int64_t n_bytes,
                      char *msg, int32_t msg_len);

int32_t ua_dll_end(void *ctx, char *msg, int32_t msg_len);

#ifdef __cplusplus
}
#endif
#endif /* UA_DLL_API_H */
