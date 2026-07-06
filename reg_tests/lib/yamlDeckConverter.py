"""
    Standalone text -> YAML converters for OpenFAST input decks, used by the
    YAML-equivalence regression tests (executeYamlEquivalenceCase.py).

    Deliberately dependency-free (no openfast_io): each converter knows one module's
    text layout and emits the documented YAML schema for it. Numeric and boolean
    values are copied as their original literal text so that Fortran's list-directed
    READ produces bit-identical values in both formats.

    Converters:
        convert_inflowwind(text_path) -> str   (YAML document)
        convert_aerodisk(text_path) -> str      (YAML document)
        convert_sed(text_path) -> str           (YAML document)
        convert_seastate(text_path) -> str      (YAML document)
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


def _list_join(toks):
    """Join value tokens for a flow-sequence entry, stripping any trailing comma each
    token may carry (comma-separated text-format lists tokenize as e.g. "0," "0," "0";
    left as-is these would double up with the ', ' separator and produce an empty
    flow-collection item, e.g. "[0,, 0,, 0]")."""
    return ', '.join(t.rstrip(',') for t in toks)


def _as_str(tok):
    s = _unquote(tok)
    # escape backslashes/quotes so paths like Windows-style "a\b" (as seen in some
    # unused placeholder fields) remain valid double-quoted YAML scalars
    s = s.replace('\\', '\\\\').replace('"', '\\"')
    return '"' + s + '"'


def _default_or_num(tok):
    """Several keys (DT_Out, and AeroDisk's DT/AirDens/RotorRad) accept a literal
    "default"/"DEFAULT" token in the text format; YamlGet's ScalarText helper
    special-cases that keyword (quoted or bare) identically, so pass it through
    bare. Otherwise keep the numeric literal verbatim."""
    unquoted = _unquote(tok)
    if unquoted.strip().lower() == 'default':
        return 'default'
    return unquoted


def _is_comment_or_blank(line):
    """NWTC_IO's ReadLine/ScanComFile treat '!', '#', and '%' as comment characters and
    drop blank or comment-only lines entirely (they are never counted/stored)."""
    s = line.strip()
    return (not s) or (s[0] in '!#%')


def _strip_inline_comment(line):
    """Truncate at the first comment character, mirroring ReadLine (which returns only
    what precedes the first comment character on a line)."""
    idx = len(line)
    for ch in '!#%':
        p = line.find(ch)
        if p != -1 and p < idx:
            idx = p
    return line[:idx]


class _TextDeck:
    """Sequential keyword-value scanner over a text-format deck."""

    def __init__(self, path):
        with open(path, 'r', errors='replace') as f:
            self.lines = f.read().splitlines()
        self.cursor = 0
        self.path = path
        self.base_dir = os.path.dirname(path) or '.'

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

    def table_rows(self, n_rows, skip=0):
        """Skip `skip` significant (non-comment/blank) lines, then collect exactly
        `n_rows` numeric table-body lines -- both counted over the same *expanded*
        line stream NWTC_IO itself sees: '@filename' file-inclusion lines (NWTC_IO's
        ProcessComFile/ScanComFile convention) are transparently spliced in, and
        comment/blank lines are dropped exactly like ReadLine does.

        `skip` and `n_rows` are counted over ONE continuous stream on purpose: some
        r-test decks give the table's two descriptive header/units lines directly in
        the primary file before a single '@include' line that then supplies only the
        data rows; others omit those two lines from the primary file entirely and
        instead put them as the first two (non-comment) lines *inside* the included
        file, immediately before its data rows. Both are legal because
        AeroDisk_IO.f90's text-format reader also just counts lines in the fully
        expanded FileInfoType, with no notion of which physical file a line came
        from -- this mirrors that exactly.

        Returns the collected rows as raw (comment-stripped, trimmed) text; advances
        the cursor past whatever was consumed in this deck's own lines (an
        '@include' line counts as one line here, however many lines it expands to)."""
        rows = []
        to_skip = skip
        i = self.cursor

        def consume(text):
            nonlocal to_skip
            if to_skip > 0:
                to_skip -= 1
                return
            if len(rows) < n_rows:
                rows.append(_strip_inline_comment(text).strip())

        while to_skip > 0 or len(rows) < n_rows:
            if i >= len(self.lines):
                raise ValueError('table_rows: ran out of lines in {} before collecting {} row(s) '
                                  '(need to skip {} more, got {} row(s))'.format(
                                      self.path, n_rows, to_skip, len(rows)))
            raw = self.lines[i]
            if _is_comment_or_blank(raw):
                i += 1
                continue
            stripped = raw.strip()
            if stripped[0] == '@':
                rest = stripped[1:].strip()
                m = re.match(r'"[^"]*"|\'[^\']*\'|\S+', rest)
                fname = _unquote(m.group(0)) if m else ''
                inc_path = os.path.join(self.base_dir, fname)
                with open(inc_path, 'r', errors='replace') as f:
                    inc_lines = f.read().splitlines()
                for iline in inc_lines:
                    if _is_comment_or_blank(iline):
                        continue
                    if to_skip <= 0 and len(rows) >= n_rows:
                        break
                    consume(iline)
                i += 1
                continue
            consume(raw)
            i += 1

        self.cursor = i
        return rows


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
    w('  WindVxiList: [' + _list_join(d.find('WindVxiList')[:n_wind_vel]) + ']')
    w('  WindVyiList: [' + _list_join(d.find('WindVyiList')[:n_wind_vel]) + ']')
    w('  WindVziList: [' + _list_join(d.find('WindVziList')[:n_wind_vel]) + ']')

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
    w('  FocalDistanceX: [' + _list_join(d.find('FocalDistanceX')[:n_beam]) + ']')
    w('  FocalDistanceY: [' + _list_join(d.find('FocalDistanceY')[:n_beam]) + ']')
    w('  FocalDistanceZ: [' + _list_join(d.find('FocalDistanceZ')[:n_beam]) + ']')
    w('  RotorApexOffsetPos: [' + _list_join(d.find('RotorApexOffsetPos')[:3]) + ']')
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


def convert_aerodisk(text_path):
    """Convert a text-format AeroDisk primary input file to its YAML schema.

    The Actuator Disk Properties table (InColNames/InColDims plus the table body -- the
    body may be given inline or via a single '@filename' inclusion line, as in the
    reg_tests r-test case) is emitted as the documented columns:/rows: table under
    actuator_disk:table. `columns` holds the index-column names in the order given by
    InColNames; `dims` holds the matching InColDims counts (N_TSR/N_RtSpd/N_VRel/N_Pitch/
    N_Skew are derived from this list in the YAML parser, not separate keys); each row in
    `rows` holds the `columns` values followed by the six fixed coefficient columns
    (C_Fx, C_Fy, C_Fz, C_Mx, C_My, C_Mz), exactly the column layout of the text-format
    table body."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# AeroDisk primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('  DT: ' + _default_or_num(d.scalar('DT')))
    w('')

    w('environmental_conditions:')
    w('  AirDens: ' + _default_or_num(d.scalar('AirDens')))
    w('')

    w('actuator_disk:')
    w('  RotorRad: ' + _default_or_num(d.scalar('RotorRad')))

    # InColNames is a single quoted, comma-separated token (e.g. "TSR, RtSpd, VRel, Skew, Pitch")
    incolnames_tok = d.scalar('InColNames')
    names = [n.strip() for n in _unquote(incolnames_tok).split(',') if n.strip()]

    # InColDims is a comma-separated list of integers, same order/length as InColNames
    incoldims_toks = d.find('InColDims')[:len(names)]
    dims = [int(t.rstrip(',')) for t in incoldims_toks]
    if len(dims) != len(names):
        raise ValueError('convert_aerodisk: InColDims has fewer entries than InColNames in {}'.format(text_path))

    n_rows = 1
    for n in dims:
        n_rows *= max(n, 1)
    n_cols = len(names) + 6   # + the 6 fixed coefficient columns C_Fx..C_Mz

    # two header/units lines follow (column names, then units) -- purely descriptive,
    # skipped without parsing exactly as AeroDisk_IO.f90 does (CurLine += 1 twice).
    # They may appear directly in the primary file, or (if the whole table is pulled
    # in via a single '@include' line) as the include file's own first two lines --
    # table_rows(skip=2) counts skip+rows over the same expanded stream so either
    # layout works.
    rows = d.table_rows(n_rows, skip=2)

    w('  table:')
    w('    columns: [' + ', '.join(names) + ']')
    w('    dims: [' + ', '.join(str(n) for n in dims) + ']')
    w('    rows:')
    for row in rows:
        toks = [t for t in re.split(r'[,\s]+', row.strip()) if t]
        if len(toks) != n_cols:
            raise ValueError('convert_aerodisk: table row "{}" in {} has {} value(s); expected {}'.format(
                row, text_path, len(toks), n_cols))
        w('      - [' + ', '.join(toks) + ']')
    w('')

    w('output:')
    channels = d.outlist()
    w('  OutList: [' + ', '.join('"' + c + '"' for c in channels) + ']')
    w('')

    return '\n'.join(out)


def convert_sed(text_path):
    """Convert a text-format Simplified ElastoDyn (SED) primary input file to its YAML
    schema.

    Sections mirror the text file's banners (general, degrees_of_freedom,
    initial_conditions, turbine_configuration, mass_and_inertia, drivetrain, output).
    Only DT accepts the scalar "default" (ParseVarWDefault in the text path); every
    other key is required. SumPrint is never read by the text-format parser either, so
    it has no YAML key. Angle/speed values (Azimuth, BlPitch, RotSpeed, NacYaw,
    PtfmPitch, PreCone, ShftTilt) are copied verbatim in their native units (deg / rpm)
    -- SED_Yaml.f90 applies the same deg->rad / rpm->rad/s conversions as the text path
    right after reading them."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# Simplified ElastoDyn (SED) primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('  IntMethod: ' + d.scalar('IntMethod'))
    w('  DT: ' + _default_or_num(d.scalar('DT')))
    w('')

    w('degrees_of_freedom:')
    w('  GenDOF: ' + _as_bool(d.scalar('GenDOF')))
    w('  YawDOF: ' + _as_bool(d.scalar('YawDOF')))
    w('')

    w('initial_conditions:')
    w('  Azimuth: '   + d.scalar('Azimuth'))
    w('  BlPitch: '   + d.scalar('BlPitch'))
    w('  RotSpeed: '  + d.scalar('RotSpeed'))
    w('  NacYaw: '    + d.scalar('NacYaw'))
    w('  PtfmPitch: ' + d.scalar('PtfmPitch'))
    w('')

    w('turbine_configuration:')
    w('  NumBl: '    + d.scalar('NumBl'))
    w('  TipRad: '   + d.scalar('TipRad'))
    w('  HubRad: '   + d.scalar('HubRad'))
    w('  PreCone: '  + d.scalar('PreCone'))
    w('  OverHang: ' + d.scalar('OverHang'))
    w('  ShftTilt: ' + d.scalar('ShftTilt'))
    w('  Twr2Shft: ' + d.scalar('Twr2Shft'))
    w('  TowerHt: '  + d.scalar('TowerHt'))
    w('')

    w('mass_and_inertia:')
    w('  RotIner: ' + d.scalar('RotIner'))
    w('  GenIner: ' + d.scalar('GenIner'))
    w('')

    w('drivetrain:')
    w('  GBoxRatio: ' + d.scalar('GBoxRatio'))
    w('')

    w('output:')
    channels = d.outlist()
    w('  OutList: [' + ', '.join('"' + c + '"' for c in channels) + ']')
    w('')

    return '\n'.join(out)


def convert_seastate(text_path):
    """Convert a text-format SeaState primary input file to its YAML schema.

    Sections mirror the text file's banners (general, environmental_conditions,
    spatial_discretization, waves, second_order_waves, constrained_waves, current,
    maccamy_fuchs, output, output_channels). WtrDens/WtrDpth/MSL2SWL/Z_Depth/WavePkShp
    accept the scalar "default" (ParseVarWDefault in the text path); CurrSSDir's
    "DEFAULT" sentinel is passed through the same helper (SeaState_Yaml.f90 recognizes
    it case-insensitively regardless of case). WaveMod and WaveSeed2 are variant fields
    (an integer, or a "1P#"/RNG-name string respectively) and are copied through
    verbatim, unquoted, exactly like the text format. NWaveElev/NWaveKin/NumOuts are
    derived in YAML from list lengths, so the text-format counts are consumed here only
    to know how many list entries to slice, and are never themselves emitted."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# SeaState primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('')

    w('environmental_conditions:')
    w('  WtrDens: ' + _default_or_num(d.scalar('WtrDens')))
    w('  WtrDpth: ' + _default_or_num(d.scalar('WtrDpth')))
    w('  MSL2SWL: ' + _default_or_num(d.scalar('MSL2SWL')))
    w('')

    w('spatial_discretization:')
    w('  X_HalfWidth: ' + d.scalar('X_HalfWidth'))
    w('  Y_HalfWidth: ' + d.scalar('Y_HalfWidth'))
    w('  Z_Depth: '     + _default_or_num(d.scalar('Z_Depth')))
    w('  NX: ' + d.scalar('NX'))
    w('  NY: ' + d.scalar('NY'))
    w('  NZ: ' + d.scalar('NZ'))
    w('')

    w('waves:')
    w('  WaveMod: '       + _unquote(d.scalar('WaveMod')))
    w('  WaveStMod: '     + d.scalar('WaveStMod'))
    w('  WvCrntMod: '     + d.scalar('WvCrntMod'))
    w('  WaveTMax: '      + d.scalar('WaveTMax'))
    w('  WaveDT: '        + d.scalar('WaveDT'))
    w('  WaveHs: '        + d.scalar('WaveHs'))
    w('  WaveTp: '        + d.scalar('WaveTp'))
    w('  WavePkShp: '     + _default_or_num(d.scalar('WavePkShp')))
    w('  WvLowCOff: '     + d.scalar('WvLowCOff'))
    w('  WvHiCOff: '      + d.scalar('WvHiCOff'))
    w('  WaveDir: '       + d.scalar('WaveDir'))
    w('  WaveDirMod: '    + d.scalar('WaveDirMod'))
    w('  WaveDirSpread: ' + d.scalar('WaveDirSpread'))
    w('  WaveNDir: '      + d.scalar('WaveNDir'))
    w('  WaveDirRange: '  + d.scalar('WaveDirRange'))
    w('  WaveSeed1: '     + d.scalar('WaveSeed(1)'))
    w('  WaveSeed2: '     + _unquote(d.scalar('WaveSeed(2)')))
    w('  WaveNDAmp: '     + _as_bool(d.scalar('WaveNDAmp')))
    w('  WvKinFile: '     + _as_str(d.scalar('WvKinFile')))
    w('')

    w('second_order_waves:')
    w('  WvDiffQTF: '  + _as_bool(d.scalar('WvDiffQTF')))
    w('  WvSumQTF: '   + _as_bool(d.scalar('WvSumQTF')))
    w('  WvLowCOffD: ' + d.scalar('WvLowCOffD'))
    w('  WvHiCOffD: '  + d.scalar('WvHiCOffD'))
    w('  WvLowCOffS: ' + d.scalar('WvLowCOffS'))
    w('  WvHiCOffS: '  + d.scalar('WvHiCOffS'))
    w('')

    w('constrained_waves:')
    w('  ConstWaveMod: ' + d.scalar('ConstWaveMod'))
    w('  CrestHmax: '    + d.scalar('CrestHmax'))
    w('  CrestTime: '    + d.scalar('CrestTime'))
    w('  CrestXi: '      + d.scalar('CrestXi'))
    w('  CrestYi: '      + d.scalar('CrestYi'))
    w('')

    w('current:')
    w('  CurrMod: '   + d.scalar('CurrMod'))
    w('  CurrSSV0: '  + d.scalar('CurrSSV0'))
    w('  CurrSSDir: ' + _default_or_num(d.scalar('CurrSSDir')))
    w('  CurrNSRef: ' + d.scalar('CurrNSRef'))
    w('  CurrNSV0: '  + d.scalar('CurrNSV0'))
    w('  CurrNSDir: ' + d.scalar('CurrNSDir'))
    w('  CurrDIV: '    + d.scalar('CurrDIV'))
    w('  CurrDIDir: ' + d.scalar('CurrDIDir'))
    w('')

    w('maccamy_fuchs:')
    w('  MCFD: ' + d.scalar('MCFD'))
    w('')

    w('output:')
    w('  SeaStSum: ' + _as_bool(d.scalar('SeaStSum')))
    w('  OutSwtch: ' + d.scalar('OutSwtch'))
    w('  OutFmt: '   + _as_str(d.scalar('OutFmt')))
    w('  OutSFmt: '  + _as_str(d.scalar('OutSFmt')))

    n_wave_elev = int(d.scalar('NWaveElev'))
    w('  WaveElevxi: [' + _list_join(d.find('WaveElevxi')[:n_wave_elev]) + ']')
    w('  WaveElevyi: [' + _list_join(d.find('WaveElevyi')[:n_wave_elev]) + ']')

    n_wave_kin = int(d.scalar('NWaveKin'))
    w('  WaveKinxi: [' + _list_join(d.find('WaveKinxi')[:n_wave_kin]) + ']')
    w('  WaveKinyi: [' + _list_join(d.find('WaveKinyi')[:n_wave_kin]) + ']')
    w('  WaveKinzi: [' + _list_join(d.find('WaveKinzi')[:n_wave_kin]) + ']')
    w('')

    w('output_channels:')
    # unlike the other modules' OUTPUT sections, SeaState's text format has no literal
    # "OutList" keyword line -- channel names start immediately after the "OUTPUT
    # CHANNELS" section banner, so anchor on the banner text itself
    channels = d.outlist(keyword='OUTPUT CHANNELS')
    w('  OutList: [' + ', '.join('"' + c + '"' for c in channels) + ']')
    w('')

    return '\n'.join(out)


def _quote_line(text):
    """Double-quote an arbitrary raw line of text (e.g. the .fst description),
    escaping backslashes/quotes so it is always a valid YAML scalar even when
    the line contains ':' or other flow-scalar-hostile characters."""
    return '"' + text.strip().replace('\\', '\\\\').replace('"', '\\"') + '"'


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
      'all-yaml'    - same, but the referenced InflowWind file (if CompInflow == 1),
                      AeroDisk file (if CompAero == 1), EDFile (if CompElast == 3, i.e.
                      Simplified ElastoDyn), and/or SeaStFile (if CompSeaSt == 1) are
                      ALSO converted to YAML and their input_files entries are
                      repointed at the new .yaml files. Other module files stay as
                      text paths.
      'single-file' - the InflowWind input (if CompInflow == 1), the AeroDisk input
                      (if CompAero == 1), the EDFile input (if CompElast == 3), and/or
                      the SeaStFile input (if CompSeaSt == 1) are inlined as a nested
                      mapping under input_files:InflowFile / input_files:AeroFile /
                      input_files:EDFile / input_files:SeaStFile (the uniform value
                      rule: a mapping value is inline module input). Other modules
                      stay as text paths.

    Returns (yaml_text, extra_files):
      yaml_text   - the YAML document for the .fst itself (str).
      extra_files - dict of {relative_filename: yaml_text} for any sibling files
                    the caller must also write out (populated only for mode
                    'all-yaml', where InflowFile/AeroFile are converted to standalone
                    sibling .yaml files).
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

    # EDFile serves ElastoDyn (CompElast 1/2) and Simplified ElastoDyn (CompElast == 3);
    # only the SED case has a YAML reader, so conversion/inlining is gated on that switch
    convert_ed = (comp_elast == 3) and (mode in ('all-yaml', 'single-file'))
    ed_rel = _unquote(ed_file)
    if convert_ed:
        ed_abs = os.path.join(base_dir, ed_rel)
        sed_yaml_text = convert_sed(ed_abs)
        if mode == 'single-file':
            w('  EDFile:')
            # drop the two leading '# ...' header comments before inlining, then
            # indent so the embedded document's top-level keys land under EDFile:
            sed_lines = sed_yaml_text.split('\n')
            sed_body = '\n'.join(sed_lines[2:]) if len(sed_lines) > 2 else sed_yaml_text
            w(_indent_block(sed_body, '    '))
        else:  # all-yaml
            sed_yaml_rel = os.path.splitext(ed_rel)[0] + '.yaml'
            extra_files[sed_yaml_rel] = sed_yaml_text
            w('  EDFile: ' + _as_str(sed_yaml_rel))
    else:
        w('  EDFile: ' + _as_str(ed_rel))

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

    convert_aero = (comp_aero == 1) and (mode in ('all-yaml', 'single-file'))
    aero_rel = _unquote(aero_file)
    if convert_aero:
        aero_abs = os.path.join(base_dir, aero_rel)
        adsk_yaml_text = convert_aerodisk(aero_abs)
        if mode == 'single-file':
            w('  AeroFile:')
            # drop the two leading '# ...' header comments before inlining, then
            # indent so the embedded document's top-level keys land under AeroFile:
            adsk_lines = adsk_yaml_text.split('\n')
            adsk_body = '\n'.join(adsk_lines[2:]) if len(adsk_lines) > 2 else adsk_yaml_text
            w(_indent_block(adsk_body, '    '))
        else:  # all-yaml
            adsk_yaml_rel = os.path.splitext(aero_rel)[0] + '.yaml'
            extra_files[adsk_yaml_rel] = adsk_yaml_text
            w('  AeroFile: ' + _as_str(adsk_yaml_rel))
    else:
        w('  AeroFile: ' + _as_str(aero_rel))

    w('  ServoFile: '   + _as_str(servo_file))

    convert_seast = (comp_seast == 1) and (mode in ('all-yaml', 'single-file'))
    seast_rel = _unquote(seast_file)
    if convert_seast:
        seast_abs = os.path.join(base_dir, seast_rel)
        seast_yaml_text = convert_seastate(seast_abs)
        if mode == 'single-file':
            w('  SeaStFile:')
            # drop the two leading '# ...' header comments before inlining, then
            # indent so the embedded document's top-level keys land under SeaStFile:
            seast_lines = seast_yaml_text.split('\n')
            seast_body = '\n'.join(seast_lines[2:]) if len(seast_lines) > 2 else seast_yaml_text
            w(_indent_block(seast_body, '    '))
        else:  # all-yaml
            seast_yaml_rel = os.path.splitext(seast_rel)[0] + '.yaml'
            extra_files[seast_yaml_rel] = seast_yaml_text
            w('  SeaStFile: ' + _as_str(seast_yaml_rel))
    else:
        w('  SeaStFile: ' + _as_str(seast_rel))

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
