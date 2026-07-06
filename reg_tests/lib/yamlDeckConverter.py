"""
    Standalone text -> YAML converters for OpenFAST input decks, used by the
    YAML-equivalence regression tests (executeYamlEquivalenceCase.py).

    Deliberately dependency-free (no openfast_io): each converter knows one module's
    text layout and emits the documented YAML schema for it. Numeric and boolean
    values are copied as their original literal text so that Fortran's list-directed
    READ produces bit-identical values in both formats.

    Converters:
        convert_inflowwind(text_path) -> str   (YAML document)
        convert_fst(text_path, mode) -> (str, dict)   (YAML document, extra sibling files)
"""

import os
import re


def _tokens_before_keyword(line, keyword):
    """Return the whitespace-separated value tokens preceding `keyword` (case-insensitive,
    word-boundary) on `line`, or None if the keyword is not on the line."""
    m = re.search(r'(?i)(?<![A-Za-z0-9_])' + re.escape(keyword) + r'(?![A-Za-z0-9_])', line)
    if m is None:
        return None
    value_text = line[:m.start()].strip()
    # tokenize honoring double-quoted strings
    toks = re.findall(r'"[^"]*"|\'[^\']*\'|\S+', value_text)
    return toks


def _unquote(tok):
    if len(tok) >= 2 and tok[0] == tok[-1] and tok[0] in "\"'":
        return tok[1:-1]
    return tok


def _as_bool(tok):
    return 'true' if _unquote(tok).strip().lower() in ('true', 't', '.true.') else 'false'


def _as_str(tok):
    s = _unquote(tok)
    # escape backslashes/quotes so paths like Windows-style "a\b" (as seen in some
    # unused placeholder fields) remain valid double-quoted YAML scalars
    s = s.replace('\\', '\\\\').replace('"', '\\"')
    return '"' + s + '"'


class _TextDeck:
    """Sequential keyword-value scanner over a text-format deck."""

    def __init__(self, path):
        with open(path, 'r', errors='replace') as f:
            self.lines = f.read().splitlines()
        self.cursor = 0
        self.path = path

    def find(self, keyword):
        """Advance to the next line containing `keyword`; return its value tokens."""
        for i in range(self.cursor, len(self.lines)):
            toks = _tokens_before_keyword(self.lines[i], keyword)
            if toks is not None:
                self.cursor = i + 1
                return toks
        raise KeyError('keyword "{}" not found in {} (after line {})'.format(
            keyword, self.path, self.cursor))

    def scalar(self, keyword):
        toks = self.find(keyword)
        if not toks:
            raise ValueError('keyword "{}" in {} has no value'.format(keyword, self.path))
        return toks[0]

    def outlist(self, keyword='OutList'):
        """Collect channel names from the lines following the OutList marker until END."""
        self.find(keyword)
        channels = []
        for i in range(self.cursor, len(self.lines)):
            stripped = self.lines[i].strip()
            if stripped[:3].upper() == 'END':
                break
            first = re.match(r'"[^"]*"|\'[^\']*\'|\S+', stripped)
            if first is None:
                continue
            for name in _unquote(first.group(0)).split(','):
                name = name.strip()
                if name:
                    channels.append(name)
        return channels


def convert_inflowwind(text_path):
    """Convert a text-format InflowWind primary input file to its YAML schema."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# InflowWind primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(text_path.split('/')[-1]))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('  WindType: ' + d.scalar('WindType'))
    w('  PropagationDir: ' + d.scalar('PropagationDir'))
    w('  VFlowAng: ' + d.scalar('VFlowAng'))
    w('  VelInterpCubic: ' + _as_bool(d.scalar('VelInterpCubic')))
    n_wind_vel = int(d.scalar('NWindVel'))
    # counts are derived from list lengths in YAML: emit exactly NWindVel entries
    w('  WindVxiList: [' + ', '.join(d.find('WindVxiList')[:n_wind_vel]) + ']')
    w('  WindVyiList: [' + ', '.join(d.find('WindVyiList')[:n_wind_vel]) + ']')
    w('  WindVziList: [' + ', '.join(d.find('WindVziList')[:n_wind_vel]) + ']')

    w('steady_wind:')
    w('  HWindSpeed: ' + d.scalar('HWindSpeed'))
    w('  RefHt: ' + d.scalar('RefHt'))
    w('  PLexp: ' + d.scalar('PLexp'))

    w('uniform_wind:')
    w('  FileName_Uni: ' + _as_str(d.scalar('FileName_Uni')))
    w('  RefHt_Uni: ' + d.scalar('RefHt_Uni'))
    w('  RefLength: ' + d.scalar('RefLength'))

    w('turbsim_wind:')
    w('  FileName_BTS: ' + _as_str(d.scalar('FileName_BTS')))

    w('bladed_wind:')
    w('  FilenameRoot: ' + _as_str(d.scalar('FilenameRoot')))
    w('  TowerFile: ' + _as_bool(d.scalar('TowerFile')))

    w('hawc_wind:')
    w('  FileName_u: ' + _as_str(d.scalar('FileName_u')))
    w('  FileName_v: ' + _as_str(d.scalar('FileName_v')))
    w('  FileName_w: ' + _as_str(d.scalar('FileName_w')))
    for key in ('nx', 'ny', 'nz', 'dx', 'dy', 'dz', 'RefHt_HAWC'):
        w('  {}: {}'.format(key, d.scalar(key)))
    w('  scaling:')
    for key in ('ScaleMethod', 'SFx', 'SFy', 'SFz', 'SigmaFx', 'SigmaFy', 'SigmaFz'):
        w('    {}: {}'.format(key, d.scalar(key)))
    w('  mean_profile:')
    w('    URef: ' + d.scalar('URef'))
    w('    WindProfile: ' + d.scalar('WindProfile'))
    w('    PLExp_HAWC: ' + d.scalar('PLExp_HAWC'))
    w('    Z0: ' + d.scalar('Z0'))
    w('    XOffset: ' + d.scalar('XOffset'))

    w('lidar:')
    w('  SensorType: ' + d.scalar('SensorType'))
    w('  NumPulseGate: ' + d.scalar('NumPulseGate'))
    w('  PulseSpacing: ' + d.scalar('PulseSpacing'))
    n_beam = max(int(d.scalar('NumBeam')), 1)   # the text path clamps NumBeam to >= 1
    w('  FocalDistanceX: [' + ', '.join(d.find('FocalDistanceX')[:n_beam]) + ']')
    w('  FocalDistanceY: [' + ', '.join(d.find('FocalDistanceY')[:n_beam]) + ']')
    w('  FocalDistanceZ: [' + ', '.join(d.find('FocalDistanceZ')[:n_beam]) + ']')
    w('  RotorApexOffsetPos: [' + ', '.join(d.find('RotorApexOffsetPos')[:3]) + ']')
    w('  URefLid: ' + d.scalar('URefLid'))
    w('  MeasurementInterval: ' + d.scalar('MeasurementInterval'))
    w('  LidRadialVel: ' + _as_bool(d.scalar('LidRadialVel')))
    w('  ConsiderHubMotion: ' + d.scalar('ConsiderHubMotion'))

    w('output:')
    w('  SumPrint: ' + _as_bool(d.scalar('SumPrint')))
    channels = d.outlist()
    w('  OutList: [' + ', '.join('"' + c + '"' for c in channels) + ']')
    w('')

    return '\n'.join(out)


def _quote_line(text):
    """Double-quote an arbitrary raw line of text (e.g. the .fst description),
    escaping backslashes/quotes so it is always a valid YAML scalar even when
    the line contains ':' or other flow-scalar-hostile characters."""
    return '"' + text.strip().replace('\\', '\\\\').replace('"', '\\"') + '"'


def _default_or_num(tok):
    """DT_Out (and similarly Default-eligible keys) accept a literal
    "default"/"DEFAULT" token in the text format; YamlGet's ScalarText helper
    special-cases that keyword (quoted or bare) identically, so pass it through
    bare. Otherwise keep the numeric literal verbatim."""
    unquoted = _unquote(tok)
    if unquoted.strip().lower() == 'default':
        return 'default'
    return unquoted


def _indent_block(text, prefix):
    """Indent every non-blank line of `text` by `prefix` (a string of spaces)."""
    lines = text.split('\n')
    return '\n'.join((prefix + line) if line.strip() else '' for line in lines)


def convert_fst(text_path, mode='per-file'):
    """Convert a text-format OpenFAST primary (.fst) input file to its YAML schema
    (modules/openfast-library/src/FAST_Yaml.f90 is the source of truth).

    mode:
      'per-file'    - all module input file entries stay as paths to the original
                      (text) files, unchanged.
      'all-yaml'    - same, but the referenced InflowWind file (if CompInflow == 1)
                      is ALSO converted to YAML and the InflowFile entry is repointed
                      at the new .yaml file. Other module files stay as text paths.
      'single-file' - the InflowWind input (if CompInflow == 1) is inlined as a
                      nested mapping under input_files:InflowFile (the uniform value
                      rule: a mapping value is inline module input). Other modules
                      stay as text paths.

    Returns (yaml_text, extra_files):
      yaml_text   - the YAML document for the .fst itself (str).
      extra_files - dict of {relative_filename: yaml_text} for any sibling files
                    the caller must also write out (populated only for mode
                    'all-yaml', where InflowFile is converted to a standalone
                    sibling .yaml file).
    """
    if mode not in ('per-file', 'all-yaml', 'single-file'):
        raise ValueError('convert_fst: unknown mode "{}"'.format(mode))

    d = _TextDeck(text_path)
    base_dir = os.path.dirname(text_path)
    extra_files = {}

    # line 1 is a pure banner comment (FAST_ReadPrimaryFile: ReadCom); line 2 is the
    # file description, read verbatim via ReadStr (no keyword, not a keyword-value line)
    description = d.lines[1] if len(d.lines) > 1 else ''
    d.cursor = 2

    out = []
    w = out.append
    w('# OpenFAST primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))
    w('description: ' + _quote_line(description))
    w('')

    #-------------------- SIMULATION CONTROL ------------------------------------
    w('simulation_control:')
    w('  Echo: '            + _as_bool(d.scalar('Echo')))
    w('  AbortLevel: '      + _as_str(d.scalar('AbortLevel')))
    w('  TMax: '            + d.scalar('TMax'))
    w('  DT: '              + d.scalar('DT'))
    w('  ModCoupling: '     + d.scalar('ModCoupling'))
    w('  InterpOrder: '     + d.scalar('InterpOrder'))
    w('  NumCrctn: '        + d.scalar('NumCrctn'))
    w('  RhoInf: '          + d.scalar('RhoInf'))
    w('  ConvTol: '         + d.scalar('ConvTol'))
    w('  MaxConvIter: '     + d.scalar('MaxConvIter'))
    w('  AutoRelax: '       + _as_bool(d.scalar('AutoRelax')))
    w('  RelaxFactor: '     + d.scalar('RelaxFactor'))
    w('  DT_UJac: '         + d.scalar('DT_UJac'))
    w('  UJacSclFact: '     + d.scalar('UJacSclFact'))
    w('')

    #-------------------- FEATURE SWITCHES AND FLAGS ----------------------------
    # NRotors is derived in YAML from input_files:rotors' length + 1; consume the
    # text-format count but do not emit it.
    n_rotors = int(d.scalar('NRotors'))

    w('feature_switches:')
    comp_elast   = int(d.scalar('CompElast'))
    comp_inflow  = int(d.scalar('CompInflow'))
    comp_aero    = int(d.scalar('CompAero'))
    comp_servo   = int(d.scalar('CompServo'))
    comp_seast   = int(d.scalar('CompSeaSt'))
    comp_hydro   = int(d.scalar('CompHydro'))
    comp_sub     = int(d.scalar('CompSub'))
    comp_mooring = int(d.scalar('CompMooring'))
    comp_ice     = int(d.scalar('CompIce'))
    comp_soil    = int(d.scalar('CompSoil'))
    mhk          = int(d.scalar('MHK'))
    w('  CompElast: '   + str(comp_elast))
    w('  CompInflow: '  + str(comp_inflow))
    w('  CompAero: '    + str(comp_aero))
    w('  CompServo: '   + str(comp_servo))
    w('  CompSeaSt: '   + str(comp_seast))
    w('  CompHydro: '   + str(comp_hydro))
    w('  CompSub: '     + str(comp_sub))
    w('  CompMooring: ' + str(comp_mooring))
    w('  CompIce: '     + str(comp_ice))
    w('  CompSoil: '    + str(comp_soil))
    w('  MHK: '         + str(mhk))

    # MirrorRotor: required (NRotors entries) only when NRotors > 1; optional and
    # commonly omitted for a single rotor, so only emit it in the multi-rotor case.
    mirror_toks = d.find('MirrorRotor')
    if n_rotors > 1:
        mirror_vals = [_as_bool(t) for t in mirror_toks[:n_rotors]]
        w('  MirrorRotor: [' + ', '.join(mirror_vals) + ']')
    w('')

    #-------------------- ENVIRONMENTAL CONDITIONS ------------------------------
    w('environment:')
    w('  Gravity: '  + d.scalar('Gravity'))
    w('  AirDens: '  + d.scalar('AirDens'))
    w('  WtrDens: '  + d.scalar('WtrDens'))
    w('  KinVisc: '  + d.scalar('KinVisc'))
    w('  SpdSound: ' + d.scalar('SpdSound'))
    w('  Patm: '     + d.scalar('Patm'))
    w('  Pvap: '     + d.scalar('Pvap'))
    w('  WtrDpth: '  + d.scalar('WtrDpth'))
    w('  MSL2SWL: '  + d.scalar('MSL2SWL'))
    w('')

    #-------------------- INPUT FILES (uniform value rule) ----------------------
    ed_file  = d.scalar('EDFile')
    bd_files = [d.scalar('BDBldFile({})'.format(k)) for k in (1, 2, 3)]
    inflow_file_tok = d.scalar('InflowFile')
    aero_file    = d.scalar('AeroFile')
    servo_file   = d.scalar('ServoFile')
    seast_file   = d.scalar('SeaStFile')
    hydro_file   = d.scalar('HydroFile')
    sub_file     = d.scalar('SubFile')
    mooring_file = d.scalar('MooringFile')
    ice_file     = d.scalar('IceFile')
    soil_file    = d.scalar('SoilFile')

    # additional rotors (multirotor): a banner + EDFile/BDBldFile(1-3)/ServoFile block
    # per rotor, same keywords as rotor 1 (FAST_ReadPrimaryFile reads them identically)
    rotors_extra = []
    for _ in range(2, n_rotors + 1):
        r_ed   = d.scalar('EDFile')
        r_bd   = [d.scalar('BDBldFile({})'.format(k)) for k in (1, 2, 3)]
        r_serv = d.scalar('ServoFile')
        rotors_extra.append((r_ed, r_bd, r_serv))

    w('input_files:')
    w('  EDFile: ' + _as_str(ed_file))
    w('  BDBldFile: [' + ', '.join(_as_str(x) for x in bd_files) + ']')

    convert_inflow = (comp_inflow == 1) and (mode in ('all-yaml', 'single-file'))
    inflow_rel = _unquote(inflow_file_tok)
    if convert_inflow:
        inflow_abs = os.path.join(base_dir, inflow_rel)
        ifw_yaml_text = convert_inflowwind(inflow_abs)
        if mode == 'single-file':
            w('  InflowFile:')
            # drop the two leading '# ...' header comments before inlining, then
            # indent so the embedded document's top-level keys land under InflowFile:
            ifw_lines = ifw_yaml_text.split('\n')
            ifw_body = '\n'.join(ifw_lines[2:]) if len(ifw_lines) > 2 else ifw_yaml_text
            w(_indent_block(ifw_body, '    '))
        else:  # all-yaml
            ifw_yaml_rel = os.path.splitext(inflow_rel)[0] + '.yaml'
            extra_files[ifw_yaml_rel] = ifw_yaml_text
            w('  InflowFile: ' + _as_str(ifw_yaml_rel))
    else:
        w('  InflowFile: ' + _as_str(inflow_rel))

    w('  AeroFile: '    + _as_str(aero_file))
    w('  ServoFile: '   + _as_str(servo_file))
    w('  SeaStFile: '   + _as_str(seast_file))
    w('  HydroFile: '   + _as_str(hydro_file))
    w('  SubFile: '     + _as_str(sub_file))
    w('  MooringFile: ' + _as_str(mooring_file))
    w('  IceFile: '     + _as_str(ice_file))
    w('  SoilFile: '    + _as_str(soil_file))

    if rotors_extra:
        w('  rotors:')
        for (r_ed, r_bd, r_serv) in rotors_extra:
            w('  - EDFile: ' + _as_str(r_ed))
            w('    BDBldFile: [' + ', '.join(_as_str(x) for x in r_bd) + ']')
            w('    ServoFile: ' + _as_str(r_serv))
    w('')

    #-------------------- OUTPUT --------------------------------------------------
    w('output:')
    w('  SumPrint: '   + _as_bool(d.scalar('SumPrint')))
    w('  SttsTime: '   + d.scalar('SttsTime'))
    w('  ChkptTime: '  + d.scalar('ChkptTime'))
    w('  DT_Out: '     + _default_or_num(d.scalar('DT_Out')))
    w('  TStart: '     + d.scalar('TStart'))
    w('  OutFileFmt: ' + d.scalar('OutFileFmt'))
    w('  TabDelim: '   + _as_bool(d.scalar('TabDelim')))
    w('  OutFmt: '     + _as_str(d.scalar('OutFmt')))
    w('')

    #-------------------- LINEARIZATION -------------------------------------------
    w('linearization:')
    w('  Linearize: '  + _as_bool(d.scalar('Linearize')))
    w('  CalcSteady: ' + _as_bool(d.scalar('CalcSteady')))
    w('  TrimCase: '   + d.scalar('TrimCase'))
    w('  TrimTol: '    + d.scalar('TrimTol'))
    w('  TrimGain: '   + d.scalar('TrimGain'))
    w('  Twr_Kdmp: '   + d.scalar('Twr_Kdmp'))
    w('  Bld_Kdmp: '   + d.scalar('Bld_Kdmp'))

    # NLinTimes is derived in YAML from the LinTimes list length; consume the
    # text-format count but only emit LinTimes (numeric list) when it's > 0, since
    # for NLinTimes == 0 the text-format line may be an unused placeholder comment
    n_lintimes = int(d.scalar('NLinTimes'))
    lintimes_toks = [t.rstrip(',') for t in d.find('LinTimes')]
    if n_lintimes > 0:
        w('  LinTimes: [' + ', '.join(lintimes_toks[-n_lintimes:]) + ']')

    w('  LinInputs: '  + d.scalar('LinInputs'))
    w('  LinOutputs: ' + d.scalar('LinOutputs'))
    w('  LinOutJac: '  + _as_bool(d.scalar('LinOutJac')))
    w('  LinOutMod: '  + _as_bool(d.scalar('LinOutMod')))
    w('')

    #-------------------- VISUALIZATION -------------------------------------------
    w('visualization:')
    w('  WrVTK: '     + d.scalar('WrVTK'))
    w('  VTK_type: '  + d.scalar('VTK_type'))
    w('  VTK_fields: ' + _as_bool(d.scalar('VTK_fields')))
    w('  VTK_fps: '   + d.scalar('VTK_fps'))
    w('')

    return '\n'.join(out), extra_files
