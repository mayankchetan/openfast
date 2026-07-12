"""
    Standalone text -> YAML converters for OpenFAST input decks, used by the
    YAML-equivalence regression tests (executeYamlEquivalenceCase.py).

    Deliberately dependency-free (no openfast_io): each converter knows one module's
    text layout and emits the documented YAML schema for it. Numeric and boolean
    values are copied as their original literal text so that Fortran's list-directed
    READ produces bit-identical values in both formats.

    Converters:
        convert_inflowwind(text_path) -> str   (YAML document)
        convert_inflowwind_driver(text_path) -> str  (YAML document; InflowWind driver file)
        convert_aerodisk(text_path) -> str      (YAML document)
        convert_aerodisk_driver(text_path) -> str  (YAML document; AeroDisk driver file)
        convert_sed(text_path) -> str           (YAML document)
        convert_sed_driver(text_path) -> str    (YAML document; SED driver file)
        convert_elastodyn(text_path) -> str     (YAML document)
        convert_beamdyn(text_path) -> str       (YAML document)
        convert_beamdyn_driver(text_path) -> str   (YAML document; BeamDyn driver file)
        convert_seastate(text_path) -> str      (YAML document)
        convert_seastate_driver(text_path) -> str  (YAML document; SeaState driver file)
        convert_servodyn(text_path, stc_to_yaml) -> (str, dict)  (YAML document, extra StC files)
        convert_stc(text_path) -> str           (YAML document)
        convert_hydrodyn(text_path) -> str      (YAML document)
        convert_hydrodyn_driver(text_path) -> str  (YAML document; HydroDyn driver file)
        convert_aerodyn(text_path, n_rotors) -> str  (YAML document)
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


def _flatten_csv(toks):
    """Flatten a list of value tokens where a comma-separated triple like "0,0,150" may
    tokenize as ONE whitespace-delimited token (the driver text format accepts either
    comma- or space-separated ReadAry lists); split every token on commas and drop any
    empty pieces (trailing commas), same convention as _list_join for double-punctuated
    input."""
    out = []
    for t in toks:
        for part in t.split(','):
            part = part.strip()
            if part:
                out.append(part)
    return out


def convert_inflowwind_driver(text_path):
    """Convert a text-format InflowWind driver input file to its YAML schema.

    Schema mirrors the driver's text reader (InflowWind_Driver_Subs.f90:700-1088,
    ReadDvrIptFile) key-for-key:
        general:              Echo
        driver_setup:         IfWIptFileName
        file_conversion:      WrHAWC, WrBladed, WrVTK, WrUniform
        interpolation_test:   NumTimeSteps, TStart, DT, Summary, SummaryFile, BoxExceedAllow
        points_file:          PointsFile, PointsFileName, CalcAccel
        gridded_data_output:  WindGrid, GridCtrCoord, GridDelta, GridN
        vtk_output:           NOutWindXY, OutWindZ

    NumTimeSteps and DT accept the literal "default"/"DEFAULT" token in the text format
    (_default_or_num passes it through bare, matching YamlGet's own "default" keyword
    handling). IfWIptFileName (driver_setup) and PointsFileName (points_file) are
    externally-referenced files and stay path-valued (second-order rule) -- never
    inlined here, matching the text path's own relative-to-PriPath resolution done in
    the Fortran reader, not here.

    The r-test driver files' column-2 labels don't always match the Fortran variable
    names one-for-one (e.g. the primary-file reference is labelled "IfWFileName" in the
    .inp text, "NumTSteps" for the timestep count, "PointsFileName" for BOTH the
    read-points-file? flag and the points filename -- read in that order, so two
    sequential .scalar('PointsFileName') calls resolve unambiguously via the cursor);
    the keywords used below match the physical label text, not ReadDvrIptFile's
    internal variable names.

    OutWindZ/GridCtrCoord/GridDelta/GridN are only present in the source line(s) when
    WindGrid/NOutWindXY require them (mirroring the text path's own conditional
    ReadAry), so they are still emitted unconditionally here (with whatever default
    values the driver file carries on those lines even when unused, since the text
    reader's ELSE branch just skips them as comments -- there is no discarded value to
    preserve either way in that branch).
    """
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# InflowWind driver input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('echo')))
    w('')

    w('driver_setup:')
    w('  IfWIptFileName: ' + _as_str(d.scalar('IfWFileName')))
    w('')

    w('file_conversion:')
    w('  WrHAWC: ' + _as_bool(d.scalar('WrHAWC')))
    w('  WrBladed: ' + _as_bool(d.scalar('WrBladed')))
    w('  WrVTK: ' + _as_bool(d.scalar('WrVTK')))
    w('  WrUniform: ' + _as_bool(d.scalar('WrUniform')))
    w('')

    w('interpolation_test:')
    w('  NumTimeSteps: ' + _default_or_num(d.scalar('NumTSteps')))
    w('  TStart: ' + d.scalar('TStart'))
    w('  DT: ' + _default_or_num(d.scalar('DT')))
    w('  Summary: ' + _as_bool(d.scalar('Summary')))
    w('  SummaryFile: ' + _as_bool(d.scalar('SummaryFile')))
    w('  BoxExceedAllow: ' + _as_bool(d.scalar('BoxExceedAllow')))
    w('')

    w('points_file:')
    # the section-header comment line itself contains "PointsFileName" as a substring
    # (e.g. "...output given as PointsFileName.Velocity.dat)..."); advance past it
    # first with a keyword unique to the comment so the two PointsFileName value reads
    # below land on the actual value lines, in the same order ReadDvrIptFile reads them
    # (flag, then filename).
    d.find('Points file input')
    w('  PointsFile: ' + _as_bool(d.scalar('PointsFileName')))
    w('  PointsFileName: ' + _as_str(d.scalar('PointsFileName')))
    w('  CalcAccel: ' + _as_bool(d.scalar('CalcAccel')))
    w('')

    w('gridded_data_output:')
    wind_grid = _as_bool(d.scalar('WindGrid'))
    w('  WindGrid: ' + wind_grid)
    # GridCtrCoord/GridDelta/GridN are only read by the YAML parser when WindGrid is
    # true (mirroring ReadDvrIptFile's IF (DvrFlags%WindGrid) branch); the text file
    # always carries these three lines regardless, but they are only emitted here when
    # WindGrid is true so an unused-but-present key never trips Yaml_WarnUnused.
    grid_ctr_toks = _flatten_csv(d.find('GridCtrCoord'))
    grid_delta_toks = _flatten_csv(d.find('GridDX'))
    grid_n_toks = _flatten_csv(d.find('GridNX'))
    if wind_grid == 'true':
        w('  GridCtrCoord: [' + _list_join(grid_ctr_toks) + ']')
        w('  GridDelta: [' + _list_join(grid_delta_toks) + ']')
        w('  GridN: [' + _list_join(grid_n_toks) + ']')
    w('')

    w('vtk_output:')
    n_out_wind_xy = int(d.scalar('NOutWindXY'))
    w('  NOutWindXY: ' + str(n_out_wind_xy))
    # OutWindZ is only read by the YAML parser when NOutWindXY > 0 (mirroring
    # ReadDvrIptFile:1038-1043); omitted here otherwise for the same unused-key reason.
    if n_out_wind_xy > 0:
        w('  OutWindZ: [' + _list_join(_flatten_csv(d.find('OutWindZ'))[:n_out_wind_xy]) + ']')
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


def convert_aerodisk_driver(text_path):
    """Convert a text-format AeroDisk driver input file (Wave 4's first module-driver
    converter -- the pattern later Wave-4 drivers reuse) to its YAML schema.

    Schema mirrors the driver's text reader (AeroDisk_Driver_Subs.f90:371-577,
    ParseDvrIptFile) key-for-key:
        general:               Echo
        primary_file:          ADskIptFile, OutRootName
        geometry_environment:  AirDens, RotorRad, RotorHeight, ShftTilt
        case_analysis:         TStart, DT, NumTimeSteps, table

    AirDens/RotorRad/RotorHeight/ShftTilt/TStart are plain ParseVar reads in the driver
    (unlike the AeroDisk *primary* file's AirDens/RotorRad, the driver's copies never
    accept the "default" keyword) -- verbatim numeric literals, no _default_or_num.
    DT and NumTimeSteps do accept "default"/"DEFAULT" (InputChr + Conv2UC + internal-READ
    in the text reader), so those two use _default_or_num.

    ADskIptFile is repointed at the YAML primary (*.inp -> *.yaml) so a driver-mode
    yaml-equivalence case (yaml driver -> yaml primary) is fully YAML end to end.

    case_analysis:table carries the combined case time/data series. Every AeroDisk
    r-test driver case sources it via a single "@filename" inclusion line (a large
    time-series file) -- second-order rule: that file is never inlined, only
    referenced by path (table: {file: "<name>"}). If the table is instead given as
    literal inline rows (no "@" line present), each row is emitted as a block-mapping
    list item under table: {rows: [...]} (SubDyn/MoorDyn primary-table precedent,
    Wave 3). A table mixing literal rows with an "@" inclusion is not supported (no
    r-test case does this; AeroDisk_Driver_Yaml.f90's Fortran reader mirrors this
    same either/or split).
    """
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# AeroDisk driver input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('')

    w('primary_file:')
    adsk_ipt = _unquote(d.scalar('ADskIptFile'))
    base, ext = os.path.splitext(adsk_ipt)
    if ext.lower() == '.inp':
        adsk_ipt = base + '.yaml'
    w('  ADskIptFile: ' + _as_str(adsk_ipt))
    w('  OutRootName: ' + _as_str(d.scalar('OutRootName')))
    w('')

    w('geometry_environment:')
    w('  AirDens: ' + d.scalar('AirDens'))
    w('  RotorRad: ' + d.scalar('RotorRad'))
    w('  RotorHeight: ' + d.scalar('RotorHeight'))
    w('  ShftTilt: ' + d.scalar('ShftTilt'))
    w('')

    w('case_analysis:')
    w('  TStart: ' + d.scalar('TStart'))
    w('  DT: ' + _default_or_num(d.scalar('DT')))
    w('  NumTimeSteps: ' + _default_or_num(d.scalar('NumTimeSteps')))

    # Two purely descriptive lines (column names, then units) follow, skipped without
    # parsing exactly as the text driver does (CurLine += 1 twice); then either a
    # single "@filename" inclusion line or literal data rows to the end of the file.
    remaining = [ln for ln in d.lines[d.cursor:] if not _is_comment_or_blank(ln)]
    if len(remaining) < 3:
        raise ValueError('convert_aerodisk_driver: {} ends before the case-analysis table '
                          '(expected 2 header lines plus at least 1 data/include line)'.format(text_path))
    data_lines = remaining[2:]

    w('  table:')
    if len(data_lines) == 1 and _strip_inline_comment(data_lines[0]).strip().startswith('@'):
        inc_line = _strip_inline_comment(data_lines[0]).strip()
        m = re.match(r'@\s*("[^"]*"|\'[^\']*\'|\S+)', inc_line)
        if m is None:
            raise ValueError('convert_aerodisk_driver: malformed "@include" line "{}" in {}'.format(
                inc_line, text_path))
        fname = _unquote(m.group(1))
        w('    file: ' + _as_str(fname))
    else:
        KEYS = ('Time', 'WndSpeed_X', 'WndSpeed_Y', 'WndSpeed_Z', 'RotSpd', 'Pitch', 'Yaw')
        w('    rows:')
        for raw in data_lines:
            stripped = _strip_inline_comment(raw).strip()
            if stripped.startswith('@'):
                raise ValueError('convert_aerodisk_driver: mixed inline/@-include case-analysis '
                                  'tables are not supported ({}).'.format(text_path))
            toks = [t for t in re.split(r'[,\s]+', stripped) if t]
            if len(toks) != 7:
                raise ValueError('convert_aerodisk_driver: case-analysis table row "{}" in {} has '
                                  '{} value(s); expected 7'.format(raw, text_path, len(toks)))
            w(_row_map(KEYS, toks))
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


def convert_sed_driver(text_path):
    """Convert a text-format Simplified ElastoDyn (SED) driver input file (Wave 4,
    mirroring convert_aerodisk_driver's pattern) to its YAML schema.

    Schema mirrors the driver's text reader (SED_Driver_Subs.f90:372-563,
    ParseDvrIptFile) key-for-key:
        general:               Echo
        primary_file:          SEDIptFile, OutRootName
        output:                WrVTK
        case_analysis:         TStart, DT, NumTimeSteps, table

    TStart is a plain ParseVar read (no "default" keyword; verbatim numeric literal).
    DT and NumTimeSteps do accept "default"/"DEFAULT" (InputChr + Conv2UC + internal-READ
    in the text reader), so those two use _default_or_num.

    SEDIptFile is repointed at the YAML primary (*.inp -> *.yaml) so a driver-mode
    yaml-equivalence case (yaml driver -> yaml primary) is fully YAML end to end.

    case_analysis:table carries the combined case time/data series (columns Time,
    AerTrq, HSSBrTrqC, GenTrq, BlPitchCom, Yaw, YawRate). Every SED r-test driver case
    sources it via a single "@filename" inclusion line (e.g. Free.csv, HSSBrk.csv) --
    second-order rule: that file is never inlined, only referenced by path (table:
    {file: "<name>"}). If the table is instead given as literal inline rows (no "@"
    line present), each row is emitted as a block-mapping list item under table:
    {rows: [...]} (SubDyn/MoorDyn primary-table precedent, Wave 3; also
    convert_aerodisk_driver's precedent within Wave 4). A table mixing literal rows
    with an "@" inclusion is not supported (no r-test case does this; SED_Driver_Yaml.f90's
    Fortran reader mirrors this same either/or split).
    """
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# Simplified ElastoDyn (SED) driver input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('')

    w('primary_file:')
    sed_ipt = _unquote(d.scalar('SEDIptFile'))
    base, ext = os.path.splitext(sed_ipt)
    if ext.lower() == '.inp':
        sed_ipt = base + '.yaml'
    w('  SEDIptFile: ' + _as_str(sed_ipt))
    w('  OutRootName: ' + _as_str(d.scalar('OutRootName')))
    w('')

    w('output:')
    w('  WrVTK: ' + d.scalar('WrVTK'))
    w('')

    w('case_analysis:')
    w('  TStart: ' + d.scalar('TStart'))
    w('  DT: ' + _default_or_num(d.scalar('DT')))
    w('  NumTimeSteps: ' + _default_or_num(d.scalar('NumTimeSteps')))

    # Two purely descriptive lines (column names, then units) follow, skipped without
    # parsing exactly as the text driver does (CurLine += 1 twice); then either a
    # single "@filename" inclusion line or literal data rows to the end of the file.
    remaining = [ln for ln in d.lines[d.cursor:] if not _is_comment_or_blank(ln)]
    if len(remaining) < 3:
        raise ValueError('convert_sed_driver: {} ends before the case-analysis table '
                          '(expected 2 header lines plus at least 1 data/include line)'.format(text_path))
    data_lines = remaining[2:]

    w('  table:')
    if len(data_lines) == 1 and _strip_inline_comment(data_lines[0]).strip().startswith('@'):
        inc_line = _strip_inline_comment(data_lines[0]).strip()
        m = re.match(r'@\s*("[^"]*"|\'[^\']*\'|\S+)', inc_line)
        if m is None:
            raise ValueError('convert_sed_driver: malformed "@include" line "{}" in {}'.format(
                inc_line, text_path))
        fname = _unquote(m.group(1))
        w('    file: ' + _as_str(fname))
    else:
        KEYS = ('Time', 'AerTrq', 'HSSBrTrqC', 'GenTrq', 'BlPitchCom', 'Yaw', 'YawRate')
        w('    rows:')
        for raw in data_lines:
            stripped = _strip_inline_comment(raw).strip()
            if stripped.startswith('@'):
                raise ValueError('convert_sed_driver: mixed inline/@-include case-analysis '
                                  'tables are not supported ({}).'.format(text_path))
            toks = [t for t in re.split(r'[,\s]+', stripped) if t]
            if len(toks) != 7:
                raise ValueError('convert_sed_driver: case-analysis table row "{}" in {} has '
                                  '{} value(s); expected 7'.format(raw, text_path, len(toks)))
            w(_row_map(KEYS, toks))
    w('')

    return '\n'.join(out)


def convert_elastodyn(text_path):
    """Convert a text-format (full) ElastoDyn primary input file to its YAML schema.

    Sections mirror the text file's banners (simulation_control, degrees_of_freedom,
    initial_conditions, turbine_configuration, mass_and_inertia, blade, rotor_teeter,
    yaw_friction, drivetrain, furling, tower, output, nodal_outputs). Only DT accepts
    the scalar "default" (the text path's "DEFAULT" special-case in ReadPrimaryFile).
    Per-blade values (BlPitch, PreCone, TipMass, PBrIner, BlPIner) -- three separate
    keyword lines in the text format (index 1..MaxBl=3) -- become 3-entry lists.
    BldFile/FurlFile/TwrFile stay path strings (second-order files: never
    converted/inlined). NTwGages/NBlGages are explicit counts (NOT list lengths --
    ElastoDyn_Yaml.f90 requires the TwrGagNd/BldGagNd lists to have at least that many
    entries, matching the text path's own count-vs-line-length contract). NumOuts and
    BldNd_NumOuts derive from output:OutList / nodal_outputs:OutList's list lengths.
    Angle/speed/percentage values are copied verbatim in their native units (deg / rpm
    / %); ElastoDyn_Yaml.f90 applies the same conversions as the text path right after
    reading them."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# ElastoDyn primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('simulation_control:')
    w('  Echo: '   + _as_bool(d.scalar('Echo')))
    w('  Method: ' + d.scalar('Method'))
    w('  DT: '     + _default_or_num(d.scalar('DT')))
    w('')

    w('degrees_of_freedom:')
    for key in ('FlapDOF1', 'FlapDOF2', 'EdgeDOF', 'PitchDOF', 'TeetDOF', 'DrTrDOF',
                'GenDOF', 'YawDOF', 'TwFADOF1', 'TwFADOF2', 'TwSSDOF1', 'TwSSDOF2',
                'PtfmSgDOF', 'PtfmSwDOF', 'PtfmHvDOF', 'PtfmRDOF', 'PtfmPDOF', 'PtfmYDOF'):
        w('  {}: {}'.format(key, _as_bool(d.scalar(key))))
    w('')

    w('initial_conditions:')
    w('  OoPDefl: ' + d.scalar('OoPDefl'))
    w('  IPDefl: '  + d.scalar('IPDefl'))
    blpitch = [d.scalar('BlPitch({})'.format(k)) for k in (1, 2, 3)]
    w('  BlPitch: [' + _list_join(blpitch) + ']')
    w('  TeetDefl: '  + d.scalar('TeetDefl'))
    w('  Azimuth: '   + d.scalar('Azimuth'))
    w('  RotSpeed: '  + d.scalar('RotSpeed'))
    w('  NacYaw: '    + d.scalar('NacYaw'))
    w('  TTDspFA: '   + d.scalar('TTDspFA'))
    w('  TTDspSS: '   + d.scalar('TTDspSS'))
    w('  PtfmSurge: ' + d.scalar('PtfmSurge'))
    w('  PtfmSway: '  + d.scalar('PtfmSway'))
    w('  PtfmHeave: ' + d.scalar('PtfmHeave'))
    w('  PtfmRoll: '  + d.scalar('PtfmRoll'))
    w('  PtfmPitch: ' + d.scalar('PtfmPitch'))
    w('  PtfmYaw: '   + d.scalar('PtfmYaw'))
    w('')

    w('turbine_configuration:')
    w('  NumBl: '   + d.scalar('NumBl'))
    w('  TipRad: '  + d.scalar('TipRad'))
    w('  HubRad: '  + d.scalar('HubRad'))
    precone = [d.scalar('PreCone({})'.format(k)) for k in (1, 2, 3)]
    w('  PreCone: [' + _list_join(precone) + ']')
    w('  HubCM: '     + d.scalar('HubCM'))
    w('  UndSling: '  + d.scalar('UndSling'))
    w('  Delta3: '    + d.scalar('Delta3'))
    w('  AzimB1Up: '  + d.scalar('AzimB1Up'))
    w('  OverHang: '  + d.scalar('OverHang'))
    w('  ShftGagL: '  + d.scalar('ShftGagL'))
    w('  ShftTilt: '  + d.scalar('ShftTilt'))
    w('  NacCMxn: '   + d.scalar('NacCMxn'))
    w('  NacCMyn: '   + d.scalar('NacCMyn'))
    w('  NacCMzn: '   + d.scalar('NacCMzn'))
    w('  NcIMUxn: '   + d.scalar('NcIMUxn'))
    w('  NcIMUyn: '   + d.scalar('NcIMUyn'))
    w('  NcIMUzn: '   + d.scalar('NcIMUzn'))
    w('  Twr2Shft: '  + d.scalar('Twr2Shft'))
    w('  TowerHt: '   + d.scalar('TowerHt'))
    w('  TowerBsHt: ' + d.scalar('TowerBsHt'))
    w('  PtfmCMxt: '  + d.scalar('PtfmCMxt'))
    w('  PtfmCMyt: '  + d.scalar('PtfmCMyt'))
    w('  PtfmCMzt: '  + d.scalar('PtfmCMzt'))
    w('  PtfmRefxt: ' + d.scalar('PtfmRefxt'))
    w('  PtfmRefyt: ' + d.scalar('PtfmRefyt'))
    w('  PtfmRefzt: ' + d.scalar('PtfmRefzt'))
    w('')

    w('mass_and_inertia:')
    tipmass = [d.scalar('TipMass({})'.format(k)) for k in (1, 2, 3)]
    w('  TipMass: [' + _list_join(tipmass) + ']')
    pbriner = [d.scalar('PBrIner({})'.format(k)) for k in (1, 2, 3)]
    w('  PBrIner: [' + _list_join(pbriner) + ']')
    blpiner = [d.scalar('BlPIner({})'.format(k)) for k in (1, 2, 3)]
    w('  BlPIner: [' + _list_join(blpiner) + ']')
    w('  HubMass: '        + d.scalar('HubMass'))
    w('  HubIner: '        + d.scalar('HubIner'))
    w('  HubIner_Teeter: ' + d.scalar('HubIner_Teeter'))
    w('  GenIner: '        + d.scalar('GenIner'))
    w('  NacMass: '        + d.scalar('NacMass'))
    w('  NacYIner: '       + d.scalar('NacYIner'))
    w('  YawBrMass: '      + d.scalar('YawBrMass'))
    w('  PtfmMass: '       + d.scalar('PtfmMass'))
    w('  PtfmRIner: '      + d.scalar('PtfmRIner'))
    w('  PtfmPIner: '      + d.scalar('PtfmPIner'))
    w('  PtfmYIner: '      + d.scalar('PtfmYIner'))
    w('  PtfmXYIner: '     + d.scalar('PtfmXYIner'))
    w('  PtfmYZIner: '     + d.scalar('PtfmYZIner'))
    w('  PtfmXZIner: '     + d.scalar('PtfmXZIner'))
    w('')

    w('blade:')
    w('  BldNodes: ' + d.scalar('BldNodes'))
    bldfiles = [_as_str(d.scalar('BldFile({})'.format(k))) for k in (1, 2, 3)]
    w('  BldFile: [' + ', '.join(bldfiles) + ']')
    w('')

    w('rotor_teeter:')
    w('  TeetMod: '  + d.scalar('TeetMod'))
    w('  TeetDmpP: ' + d.scalar('TeetDmpP'))
    w('  TeetDmp: '  + d.scalar('TeetDmp'))
    w('  TeetCDmp: ' + d.scalar('TeetCDmp'))
    w('  TeetSStP: ' + d.scalar('TeetSStP'))
    w('  TeetHStP: ' + d.scalar('TeetHStP'))
    w('  TeetSSSp: ' + d.scalar('TeetSSSp'))
    w('  TeetHSSp: ' + d.scalar('TeetHSSp'))
    w('')

    w('yaw_friction:')
    w('  YawFrctMod: ' + d.scalar('YawFrctMod'))
    w('  M_CSmax: '    + d.scalar('M_CSmax'))
    w('  M_FCSmax: '   + d.scalar('M_FCSmax'))
    w('  M_MCSmax: '   + d.scalar('M_MCSmax'))
    w('  M_CD: '       + d.scalar('M_CD'))
    w('  M_FCD: '      + d.scalar('M_FCD'))
    w('  M_MCD: '      + d.scalar('M_MCD'))
    w('  sig_v: '      + d.scalar('sig_v'))
    w('  sig_v2: '     + d.scalar('sig_v2'))
    w('  OmgCut: '     + d.scalar('OmgCut'))
    w('')

    w('drivetrain:')
    w('  GBoxEff: '  + d.scalar('GBoxEff'))
    w('  GBRatio: '  + d.scalar('GBRatio'))
    w('  DTTorSpr: ' + d.scalar('DTTorSpr'))
    w('  DTTorDmp: ' + d.scalar('DTTorDmp'))
    w('')

    w('furling:')
    w('  Furling: '  + _as_bool(d.scalar('Furling')))
    w('  FurlFile: ' + _as_str(d.scalar('FurlFile')))
    w('')

    w('tower:')
    w('  TwrNodes: ' + d.scalar('TwrNodes'))
    w('  TwrFile: '  + _as_str(d.scalar('TwrFile')))
    w('')

    w('output:')
    w('  SumPrint: ' + _as_bool(d.scalar('SumPrint')))
    w('  OutFile: '  + d.scalar('OutFile'))
    w('  TabDelim: ' + _as_bool(d.scalar('TabDelim')))
    w('  OutFmt: '   + _as_str(d.scalar('OutFmt')))
    w('  Tstart: '   + d.scalar('Tstart'))
    w('  DecFact: '  + d.scalar('DecFact'))
    w('  NTwGages: ' + d.scalar('NTwGages'))
    w('  TwrGagNd: [' + _list_join(d.find('TwrGagNd')) + ']')
    w('  NBlGages: ' + d.scalar('NBlGages'))
    w('  BldGagNd: [' + _list_join(d.find('BldGagNd')) + ']')
    channels = d.outlist()
    w('  OutList: [' + ', '.join('"' + c + '"' for c in channels) + ']')
    w('')

    # nodal_outputs is optional -- some r-test decks omit the trailing "additional
    # nodal outputs" section entirely, matching ElastoDyn_Yaml.f90's own tolerate-a-
    # missing-section fallback (BldNd_NumOuts/BldNd_BladesOut just stay 0 rather than
    # erroring); only emit the section here when the keyword is actually present in
    # the remaining, unconsumed text (mirrors BeamDyn_Yaml.f90/convert_beamdyn's
    # identical optional-section guard)
    remaining = '\n'.join(d.lines[d.cursor:])
    if re.search(r'(?i)(?<![A-Za-z0-9_])BldNd_BladesOut(?![A-Za-z0-9_])', remaining):
        w('nodal_outputs:')
        w('  BldNd_BladesOut: ' + d.scalar('BldNd_BladesOut'))
        w('  BldNd_BlOutNd: '   + _as_str(d.scalar('BldNd_BlOutNd')))
        nd_channels = d.outlist()
        w('  OutList: [' + ', '.join('"' + c + '"' for c in nd_channels) + ']')
        w('')

    return '\n'.join(out)


def _default_or_bool(tok):
    """load_retries/NRMax/stop_tol/refine/n_fact/DTBeam/tngt_stf_* (BeamDyn) accept the
    scalar "default"; the boolean ones (tngt_stf_fd, tngt_stf_comp) still need a valid
    YAML boolean literal when not defaulted, since BeamDyn_Yaml.f90 reads the raw scalar
    text back with a plain Fortran logical READ (which accepts true/false/T/F/.TRUE./
    .FALSE. alike)."""
    unquoted = _unquote(tok)
    if unquoted.strip().lower() == 'default':
        return 'default'
    return _as_bool(tok)


#-------------------- SubDyn column-count constants (SD_FEM.f90) ---------------------
_SD_JOINTS_COL   = 9   # JointID, JointXss, JointYss, JointZss, JointType, JointDirX/Y/Z, JointStiff
_SD_PROPSETSBC_COL = 6   # PropSetID, YoungE, ShearG, MatDens, XsecD, XsecT
_SD_PROPSETSBR_COL = 7   # PropSetID, YoungE, ShearG, MatDens, XsecSa, XsecSb, XsecT
_SD_PROPSETSX_COL  = 11  # PropSetID, YoungE, ShearG, MatDens, XsecA, XsecAsx, XsecAsy, XsecJxx, XsecJyy, XsecJ0, XsecJt
_SD_PROPSETSR_COL  = 2   # PropSetID, MatDens
_SD_PROPSETSS_COL  = 22  # PropSetID, k11..k66 (21 stiffness terms)
_SD_COSM_COL       = 10  # COSMID, COSM11..COSM33

# MType codes (SD_FEM.f90): beam-type members carry MSpin as their last field; all
# others (cable, rigid, or any other/spring value) carry COSMID instead.
_SD_MTYPE_BEAM = (1, -1, 4)  # idMemberBeamCirc, idMemberBeamRect, idMemberBeamArb


def _sd_mtype_code(tok):
    """Normalize a SubDyn MType token ("1c"/"1r"/an integer) to its integer code."""
    unquoted = _unquote(tok).strip()
    up = unquoted.upper()
    if up == '1C':
        return 1
    if up == '1R':
        return -1
    return int(float(unquoted))


def convert_subdyn(text_path):
    """Convert a text-format SubDyn primary input file to its YAML schema
    (modules/subdyn/src/SubDyn_Yaml.f90 is the source of truth).

    Sections mirror the text file's banners: simulation_control,
    fea_and_craig_bampton, guyan_damping, initial_rigid_body_position,
    structure_joints, base_reaction_joints, interface_joints, members,
    member_cross_section_properties, cable_properties, rigid_link_properties,
    spring_element_properties, member_direction_cosine_matrices,
    concentrated_masses, output, member_output_list, outputs.

    NJoints/nNodes_C/nNodes_I/NMembers/NPropSetsBC/BR/X/C/R/S/NCOSMs/nCMass/
    NMOutputs/NumOuts are NOT emitted -- all derive from list lengths, per
    SubDyn_Yaml.f90's module header. Joints, the three beam cross-section-property
    tables, the rigid-link and spring-element property tables, and the cosine-matrix
    table are plain fixed-column numeric row lists (flow-sequence rows); reactions
    (optional SSIfile), interfaces (optional TPIdx), members (MSpin vs. COSMID
    depending on MType), cable properties (optional CtrlChannel), concentrated
    masses (optional off-diagonal terms), and member_output_list rows are written
    as block-mapping list items keyed by column name."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# SubDyn primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    #-------------------- simulation_control -------------------------------------
    w('simulation_control:')
    w('  Echo: '      + _as_bool(d.scalar('Echo')))
    w('  SDdeltaT: '  + _default_or_num(d.scalar('SDdeltaT')))
    w('  IntMethod: ' + d.scalar('IntMethod'))
    # SttcSolve accepts either an integer or a logical (text is read into a plain
    # character buffer and dispatched with is_numeric/is_logical on both paths) --
    # pass the original literal through unchanged, exactly like SttcSolve's
    # dual-typed sibling fields elsewhere in the project (_default_or_num-style
    # pass-through) rather than coercing it to a YAML bool or number.
    w('  SttcSolve: ' + _unquote(d.scalar('SttcSolve')))
    w('')

    #-------------------- fea_and_craig_bampton -----------------------------------
    w('fea_and_craig_bampton:')
    w('  FEMMod: ' + d.scalar('FEMMod'))
    w('  NDiv: '   + d.scalar('NDiv'))
    w('  Nmodes: ' + d.scalar('Nmodes'))
    jdampings = d.find('JDampings')
    w('  JDampings: [' + _list_join(jdampings) + ']')
    w('')

    #-------------------- guyan_damping --------------------------------------------
    # Always present in the modern (non-legacy) text format this converter targets;
    # the text reader reads GuyanDampMod/RayleighDamp/GuyanDampSize/GuyanDampMat
    # unconditionally whenever the GuyanDampMod line parses as numeric, regardless
    # of its value (0 still reads -- and requires -- a full GuyanDampSize x
    # GuyanDampSize matrix).
    w('guyan_damping:')
    w('  GuyanDampMod: ' + d.scalar('GuyanDampMod'))
    rayleigh_damp = d.find('RayleighDamp')
    w('  RayleighDamp: [' + _list_join(rayleigh_damp) + ']')
    guyan_size = int(d.scalar('GuyanDampSize'))
    w('  GuyanDampSize: ' + str(guyan_size))
    guyan_rows = _matrix_rows(d, guyan_size, guyan_size, skip=0)
    w('  GuyanDampMat:')
    for row in guyan_rows:
        w('    - [' + ', '.join(row) + ']')
    w('')

    #-------------------- initial_rigid_body_position ------------------------------
    # No anchor keyword on the data line itself; skip the section banner and the
    # two header/units lines that precede it (3 significant lines).
    qr0_row = _matrix_rows(d, 1, 6, skip=3)[0]
    w('initial_rigid_body_position:')
    w('  qR0: [' + ', '.join(qr0_row) + ']')
    w('')

    #-------------------- structure_joints ------------------------------------------
    n_joints = int(d.scalar('NJoints'))
    joint_rows = _matrix_rows(d, n_joints, _SD_JOINTS_COL, skip=2)
    w('structure_joints:')
    w('  joints:')
    for row in joint_rows:
        w('    - [' + ', '.join(row) + ']')
    w('')

    #-------------------- base_reaction_joints --------------------------------------
    REACT_KEYS = ('JointID', 'Rctx', 'Rcty', 'Rctz', 'Rctxss', 'Rctyss', 'Rctzss', 'SSIfile')
    n_react = int(d.scalar('NReact'))
    react_rows = d.table_rows(n_react, skip=2)
    w('base_reaction_joints:')
    if n_react > 0:
        w('  reactions:')
        for raw in react_rows:
            toks = _row_tokens(raw)
            if len(toks) == 8:
                toks = toks[:7] + [_as_str(toks[7])]
            elif len(toks) != 7:
                raise ValueError('convert_subdyn: reaction row "{}" in {} has {} value(s); expected 7 or 8'.format(
                    raw, text_path, len(toks)))
            w(_row_map(REACT_KEYS, toks))
    else:
        w('  reactions: []')
    w('')

    #-------------------- interface_joints -------------------------------------------
    # Only JointID and TPIdx are exposed (the six DOF flags are always written as
    # "locked to TP" internally; the text path fatally rejects anything else).
    INTERF_KEYS = ('JointID', 'TPIdx')
    n_interf = int(d.scalar('NInterf'))
    interf_rows = d.table_rows(n_interf, skip=2)
    w('interface_joints:')
    if n_interf > 0:
        w('  interfaces:')
        for raw in interf_rows:
            toks = _row_tokens(raw)
            if len(toks) < 2:
                raise ValueError('convert_subdyn: interface row "{}" in {} has {} value(s); expected >= 2'.format(
                    raw, text_path, len(toks)))
            w(_row_map(INTERF_KEYS, toks[:2]))
    else:
        w('  interfaces: []')
    w('')

    #-------------------- members ----------------------------------------------------
    n_members = int(d.scalar('NMembers'))
    member_rows = d.table_rows(n_members, skip=2)
    w('members:')
    w('  members:')
    for raw in member_rows:
        toks = _row_tokens(raw)
        if len(toks) != 7:
            raise ValueError('convert_subdyn: members row "{}" in {} has {} value(s); expected 7'.format(
                raw, text_path, len(toks)))
        mtype_code = _sd_mtype_code(toks[5])
        last_key = 'MSpin' if mtype_code in _SD_MTYPE_BEAM else 'COSMID'
        keys = ('MemberID', 'MJointID1', 'MJointID2', 'MPropSetID1', 'MPropSetID2', 'MType', last_key)
        # MType stays exactly as written ("1c"/"1r"/int); only its case/spelling is
        # preserved verbatim so SD_ParseMembers' own "1C"/"1R" dispatch sees the same text
        w(_row_map(keys, toks))
    w('')

    #-------------------- member_cross_section_properties + cable/rigid/spring -------
    n_bc = int(d.scalar('NPropSetsCyl'))
    bc_rows = _matrix_rows(d, n_bc, _SD_PROPSETSBC_COL, skip=2)
    w('member_cross_section_properties:')
    if n_bc > 0:
        w('  circular_beam_props:')
        for row in bc_rows:
            w('    - [' + ', '.join(row) + ']')
    else:
        w('  circular_beam_props: []')

    n_br = int(d.scalar('NPropSetsRec'))
    br_rows = _matrix_rows(d, n_br, _SD_PROPSETSBR_COL, skip=2)
    if n_br > 0:
        w('  rectangular_beam_props:')
        for row in br_rows:
            w('    - [' + ', '.join(row) + ']')
    else:
        w('  rectangular_beam_props: []')

    n_x = int(d.scalar('NXPropSets'))
    x_rows = _matrix_rows(d, n_x, _SD_PROPSETSX_COL, skip=2)
    if n_x > 0:
        w('  arbitrary_beam_props:')
        for row in x_rows:
            w('    - [' + ', '.join(row) + ']')
    else:
        w('  arbitrary_beam_props: []')
    w('')

    CABLE_KEYS = ('PropSetID', 'EA', 'MatDens', 'T0', 'CtrlChannel')
    n_cable = int(d.scalar('NCablePropSets'))
    cable_rows = d.table_rows(n_cable, skip=2)
    w('cable_properties:')
    if n_cable > 0:
        w('  cables:')
        for raw in cable_rows:
            toks = _row_tokens(raw)
            if len(toks) not in (4, 5):
                raise ValueError('convert_subdyn: cable row "{}" in {} has {} value(s); expected 4 or 5'.format(
                    raw, text_path, len(toks)))
            w(_row_map(CABLE_KEYS, toks))
    else:
        w('  cables: []')
    w('')

    n_rigid = int(d.scalar('NRigidPropSets'))
    rigid_rows = _matrix_rows(d, n_rigid, _SD_PROPSETSR_COL, skip=2)
    w('rigid_link_properties:')
    if n_rigid > 0:
        w('  rigid_props:')
        for row in rigid_rows:
            w('    - [' + ', '.join(row) + ']')
    else:
        w('  rigid_props: []')
    w('')

    n_spring = int(d.scalar('NSpringPropSets'))
    spring_rows = _matrix_rows(d, n_spring, _SD_PROPSETSS_COL, skip=2)
    w('spring_element_properties:')
    if n_spring > 0:
        w('  spring_props:')
        for row in spring_rows:
            w('    - [' + ', '.join(row) + ']')
    else:
        w('  spring_props: []')
    w('')

    #-------------------- member_direction_cosine_matrices ----------------------------
    n_cosm = int(d.scalar('NCOSMs'))
    cosm_rows = _matrix_rows(d, n_cosm, _SD_COSM_COL, skip=2)
    w('member_direction_cosine_matrices:')
    if n_cosm > 0:
        w('  cosm:')
        for row in cosm_rows:
            w('    - [' + ', '.join(row) + ']')
    else:
        w('  cosm: []')
    w('')

    #-------------------- concentrated_masses -----------------------------------------
    CMASS_KEYS = ('JointID', 'JMass', 'JMXX', 'JMYY', 'JMZZ', 'JMXY', 'JMXZ', 'JMYZ', 'CGX', 'CGY', 'CGZ')
    n_cmass = int(d.scalar('NCmass'))
    cmass_rows = d.table_rows(n_cmass, skip=2)
    w('concentrated_masses:')
    if n_cmass > 0:
        w('  masses:')
        for raw in cmass_rows:
            toks = _row_tokens(raw)
            if len(toks) not in (5, 11):
                raise ValueError('convert_subdyn: concentrated mass row "{}" in {} has {} value(s); expected 5 or 11'.format(
                    raw, text_path, len(toks)))
            w(_row_map(CMASS_KEYS, toks))
    else:
        w('  masses: []')
    w('')

    #-------------------- output --------------------------------------------------------
    w('output:')
    w('  SumPrint: '    + _as_bool(d.scalar('SumPrint')))
    w('  OutCBModes: '  + d.scalar('OutCBModes'))
    w('  OutFEMModes: ' + d.scalar('OutFEMModes'))
    w('  OutCOSM: '     + _as_bool(d.scalar('OutCOSM')))
    w('  OutAll: '      + _as_bool(d.scalar('OutAll')))
    w('  OutSwtch: '    + d.scalar('OutSwtch'))
    w('  TabDelim: '    + _as_bool(d.scalar('TabDelim')))
    w('  OutDec: '      + d.scalar('OutDec'))
    w('  OutFmt: '      + _as_str(d.scalar('OutFmt')))
    w('  OutSFmt: '     + _as_str(d.scalar('OutSFmt')))
    w('')

    #-------------------- member_output_list ---------------------------------------------
    n_mout = int(d.scalar('NMOutputs'))
    mout_rows = d.table_rows(n_mout, skip=2)
    w('member_output_list:')
    if n_mout > 0:
        w('  members:')
        for raw in mout_rows:
            toks = _row_tokens(raw)
            if len(toks) < 3:
                raise ValueError('convert_subdyn: member_output_list row "{}" in {} has {} value(s); expected >= 3'.format(
                    raw, text_path, len(toks)))
            member_id = toks[0]
            n_out_cnt = int(toks[1])
            node_cnt = toks[2:2 + n_out_cnt]
            w('    - MemberID: ' + member_id)
            w('      NodeCnt: [' + ', '.join(node_cnt) + ']')
    else:
        w('  members: []')
    w('')

    #-------------------- outputs (SSOutList) --------------------------------------------
    # The banner line itself ("...SSOutList: The next line(s)...") is the only anchor;
    # there is no standalone "OutList" keyword line in SubDyn's text format.
    channels = d.outlist(keyword='SSOutList')
    w('outputs:')
    w('  OutList: [' + ', '.join('"' + c + '"' for c in channels) + ']')

    return '\n'.join(out)


def convert_subdyn_driver(text_path):
    """Convert a text-format SubDyn driver input file (Wave 4, class-B/sequential-
    reader driver -- follows the seastate/inflowwind convention) to its YAML schema
    (modules/subdyn/src/SubDyn_Driver_Yaml.f90 is the source of truth).

    Schema mirrors the driver's text reader (SubDyn_Driver.f90:359-479,
    ReadDriverInputFile) key-for-key:
        general:                   Echo
        environmental_conditions:  Gravity, WtrDpth
        subdyn:                    SDInputFile, OutRootName, NSteps, TimeInterval,
                                    tp_ref_points (list of {x,y,z}; its length is NTPs
                                    -- counts derive from list lengths, never a
                                    separate key), SubRotateZ
        inputs:                    InputsMod, InputsFile
        steady_state_inputs:       uTPInSteady, uDotTPInSteady, uDotDotTPInSteady --
                                    emitted ONLY when InputsMod == 1, mirroring the
                                    text path's IF (InputsMod == 1) branch (the ELSE
                                    branch's three all-zero lines carry no information
                                    worth preserving)
        loads:                     applied_loads (list of row mappings, empty list
                                    when nAppliedLoads == 0)

    SDInputFile/OutRootName/InputsFile stay path-valued (second-order rule; SDInputFile
    is NOT repointed at a .yaml sibling here -- that repointing, when needed, is done
    by the caller after this converter returns, mirroring the seastate driver-mode
    harness's own convention). Each applied-load row's optional trailing UnsteadyFile
    column is emitted only when present (8-token rows); its 7-token siblings omit the
    key entirely, matching SD_ParseAppliedLoads' Default=''."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# SubDyn driver input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('')

    w('environmental_conditions:')
    w('  Gravity: ' + d.scalar('Gravity'))
    w('  WtrDpth: ' + d.scalar('WtrDpth'))
    w('')

    w('subdyn:')
    w('  SDInputFile: ' + _as_str(d.scalar('SDInputFile')))
    w('  OutRootName: ' + _as_str(d.scalar('OutRootName')))
    w('  NSteps: ' + d.scalar('NSteps'))
    w('  TimeInterval: ' + d.scalar('TimeInterval'))
    n_tp = int(d.scalar('NTPs'))
    tp_x = _flatten_csv(d.find('TP_RefPoint_X'))[:n_tp]
    tp_y = _flatten_csv(d.find('TP_RefPoint_Y'))[:n_tp]
    tp_z = _flatten_csv(d.find('TP_RefPoint_Z'))[:n_tp]
    w('  tp_ref_points:')
    TP_KEYS = ('x', 'y', 'z')
    for i in range(n_tp):
        w(_row_map(TP_KEYS, (tp_x[i], tp_y[i], tp_z[i])))
    w('  SubRotateZ: ' + d.scalar('SubRotateZ'))
    w('')

    w('inputs:')
    inputs_mod = int(d.scalar('InputsMod'))
    w('  InputsMod: ' + str(inputs_mod))
    w('  InputsFile: ' + _as_str(d.scalar('InputsFile')))
    w('')

    # STEADY INPUTS header + the 3 fixed uTPInSteady/uDotTPInSteady/uDotDotTPInSteady
    # data lines are always physically present in the text file (read as either real
    # ReadAry data or ReadCom placeholder comments depending on InputsMod), so the 3
    # rows must always be consumed here to keep the cursor synchronized for the LOADS
    # section that follows -- emitted into the YAML only when InputsMod == 1.
    steady_rows = _matrix_rows(d, 3, 6, skip=1)
    if inputs_mod == 1:
        w('steady_state_inputs:')
        w('  uTPInSteady: [' + ', '.join(steady_rows[0]) + ']')
        w('  uDotTPInSteady: [' + ', '.join(steady_rows[1]) + ']')
        w('  uDotDotTPInSteady: [' + ', '.join(steady_rows[2]) + ']')
        w('')

    LOAD_KEYS = ('JointID', 'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz', 'UnsteadyFile')
    n_loads_toks = d.find('nAppliedLoads')
    n_loads = int(n_loads_toks[0])
    load_rows = d.table_rows(n_loads, skip=2)
    w('loads:')
    if n_loads > 0:
        w('  applied_loads:')
        for raw in load_rows:
            toks = _row_tokens(raw)
            if len(toks) == 8:
                toks = toks[:7] + [_as_str(toks[7])]
            elif len(toks) != 7:
                raise ValueError('convert_subdyn_driver: applied-load row "{}" in {} has {} value(s); expected 7 or 8'.format(
                    raw, text_path, len(toks)))
            w(_row_map(LOAD_KEYS, toks))
    else:
        w('  applied_loads: []')
    w('')

    return '\n'.join(out)


def convert_extptfm(text_path):
    """Convert a text-format ExtPtfm_MCKF primary input file to its YAML schema
    (modules/extptfm/src/ExtPtfm_Yaml.f90 is the source of truth).

    Sections mirror the text file's banners: simulation_control, reduction_inputs,
    connections, user_forcing, output, outputs.

    NActiveDOFList/NInitPosList/NInitVelList/NumOuts are NOT emitted -- counts derive
    from list lengths, per ExtPtfm_Yaml.f90's header. The large Guyan/Craig-Bampton
    reduced-data file (Red_FileName) and the connection/forcing time-series files
    (Conn_FileName, Force_FileName, FConn_FileName) all stay path strings -- they are
    second-order files read separately in ReadPrimaryFile's shared post-processing.

    The three optional lists are emitted so that ExtPtfm_Yaml's allocation semantics
    reproduce the text path's allocated/unallocated state exactly:
      - ActiveCBDOF: emitted only when NActiveDOFList >= 0 (a value < 0 means "all CB
        modes active", which the reader represents as the key being absent). When
        NActiveDOFList == 0 an empty list [] is emitted (Guyan modes only).
      - InitPosList / InitVelList: emitted only when their count is > 0 (a count <= 0
        means "all DOF initialized to 0", represented by the key being absent)."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# ExtPtfm_MCKF primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    #-------------------- simulation_control -------------------------------------
    w('simulation_control:')
    w('  Echo: '      + _as_bool(d.scalar('Echo')))
    w('  DT: '        + _default_or_num(d.scalar('DT')))
    w('  IntMethod: ' + d.scalar('IntMethod'))
    w('')

    #-------------------- reduction_inputs ---------------------------------------
    w('reduction_inputs:')
    w('  RBMod: '   + d.scalar('RBMod'))
    w('  RedFile: ' + _as_str(d.scalar('Red_FileName')))
    # NActiveDOFList then the ActiveDOFList value line (blank when NActiveDOFList < 0).
    n_active = int(d.scalar('NActiveDOFList'))
    active_toks = d.find('ActiveDOFList')
    if n_active >= 0:
        w('  ActiveCBDOF: [' + _list_join(active_toks[:n_active]) + ']')
    # NInitPosList then the InitPosList value line.
    n_initpos = int(d.scalar('NInitPosList'))
    initpos_toks = d.find('InitPosList')
    if n_initpos > 0:
        w('  InitPosList: [' + _list_join(initpos_toks[:n_initpos]) + ']')
    # NInitVelList then the InitVelList value line.
    n_initvel = int(d.scalar('NInitVelList'))
    initvel_toks = d.find('InitVelList')
    if n_initvel > 0:
        w('  InitVelList: [' + _list_join(initvel_toks[:n_initvel]) + ']')
    w('')

    #-------------------- connections --------------------------------------------
    w('connections:')
    w('  HasConnections: ' + _as_bool(d.scalar('Connections')))
    w('  ConnFile: '       + _as_str(d.scalar('Conn_FileName')))
    w('')

    #-------------------- user_forcing -------------------------------------------
    w('user_forcing:')
    w('  HasUserForcing: ' + _as_bool(d.scalar('UserForcing')))
    w('  ForceFile: '      + _as_str(d.scalar('Force_FileName')))
    w('  HasConnForcing: ' + _as_bool(d.scalar('ConnForcing')))
    w('  FConnFile: '      + _as_str(d.scalar('FConn_FileName')))
    w('')

    #-------------------- output -------------------------------------------------
    w('output:')
    w('  SumPrint: ' + _as_bool(d.scalar('SumPrint')))
    w('  OutFile: '  + d.scalar('OutFile'))
    w('  TabDelim: ' + _as_bool(d.scalar('TabDelim')))
    w('  OutFmt: '   + _as_str(d.scalar('OutFmt')))
    w('  Tstart: '   + d.scalar('TStart'))
    w('')

    #-------------------- outputs (OutList) --------------------------------------
    channels = d.outlist(keyword='OutList')
    w('outputs:')
    w('  OutList: [' + ', '.join('"' + c + '"' for c in channels) + ']')

    return '\n'.join(out)


def convert_feamooring(text_path):
    """Convert a text-format FEAMooring primary input file to its YAML schema
    (modules/feamooring/src/FEAM_Yaml.f90 is the source of truth).

    Sections mirror the text file's banners: simulation_control, lines (one mapping per
    mooring line), output, outputs.

    NumLines/NumOuts are NOT emitted -- counts derive from list lengths, per
    FEAM_Yaml.f90's header (NumLines == len(lines), NumOuts == len(outputs:OutList)).
    NumElems is kept as a scalar (it is the finite-element count per line, not a list
    count). DT/Gravity/WtrDens pass a literal "default" through, exactly like the text
    path (the reader keeps the glue code's pre-seeded value in that case).

    FEAMooring's primary input references no further data files, so nothing stays a
    path -- every field is inlined. The per-line azimuth angles (LAngAnch, LAngFair)
    are emitted in raw degrees, exactly as they appear in the text file; the reader's
    shared post-processing applies the deg->rad conversion once for both formats."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# FEAMooring primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    #-------------------- simulation_control -------------------------------------
    # Read in the text file's order (the sequential scanner advances a cursor):
    # Echo, DT, NumLines, NumElem, Gravity, WtrDens, MaxIter, Eps.
    echo    = d.scalar('Echo')
    dt      = d.scalar('DT')
    n_lines = int(d.scalar('NumLines'))          # not emitted -- derives from len(lines)
    numelem = d.scalar('NumElem')                # text label is "NumElem"
    gravity = d.scalar('Gravity')
    wtrdens = d.scalar('WtrDens')
    maxiter = d.scalar('MaxIter')
    eps     = d.scalar('Eps')
    w('simulation_control:')
    w('  Echo: '     + _as_bool(echo))
    w('  DT: '       + _default_or_num(dt))
    w('  NumElems: ' + numelem)
    w('  Gravity: '  + _default_or_num(gravity))
    w('  WtrDens: '  + _default_or_num(wtrdens))
    w('  MaxIter: '  + maxiter)
    w('  Eps: '      + eps)
    w('')

    #-------------------- lines (one mapping per mooring line) -------------------
    w('lines:')
    for _ in range(n_lines):
        w('  - LEAStiff: '  + d.scalar('LEAStiff'))
        w('    LMassDen: '  + d.scalar('LMassDen'))
        w('    LDMassDen: ' + d.scalar('LDMassDen'))
        w('    LineCI: '    + d.scalar('LineCI'))
        w('    LineCD: '    + d.scalar('LineCD'))
        w('    LUnstrLen: ' + d.scalar('LUnstrLen'))
        w('    BottmStiff: '+ d.scalar('BottmStiff'))
        w('    LRadAnch: '  + d.scalar('LRadAnch'))
        w('    LAngAnch: '  + d.scalar('LAngAnch'))
        w('    LDpthAnch: ' + d.scalar('LDpthAnch'))
        w('    LRadFair: '  + d.scalar('LRadFair'))
        w('    LAngFair: '  + d.scalar('LAngFair'))
        w('    LDrftFair: ' + d.scalar('LDrftFair'))
        w('    Tension: '   + d.scalar('Tension'))
        # GSL: the three linear spring stiffnesses (text labels GSL21/GSL22/GSL23)
        gsl = [d.scalar('GSL21'), d.scalar('GSL22'), d.scalar('GSL23')]
        w('    GSL: [' + _list_join(gsl) + ']')
    w('')

    #-------------------- output -------------------------------------------------
    w('output:')
    w('  SumPrint: ' + _as_bool(d.scalar('SumPrint')))
    w('  OutFile: '  + d.scalar('OutFile'))
    w('  TabDelim: ' + _as_bool(d.scalar('TabDelim')))
    w('  OutFmt: '   + _as_str(d.scalar('OutFmt')))
    w('  Tstart: '   + d.scalar('TStart'))
    w('')

    #-------------------- outputs (OutList) --------------------------------------
    channels = d.outlist(keyword='OutList')
    w('outputs:')
    w('  OutList: [' + ', '.join('"' + c + '"' for c in channels) + ']')

    return '\n'.join(out)


def convert_icedyn(text_path):
    """Convert a text-format IceDyn primary input file to its YAML schema
    (modules/icedyn/src/IceDyn_Yaml.f90 is the source of truth).

    Sections mirror the text file's banners: structure_properties, ice_models,
    ice_general, ice_model_1 .. ice_model_6.

    NumLegs is NOT emitted -- it derives from len(structure_properties:LegPosX), per
    IceDyn_Yaml.f90's header. There is no OutList section: IceD_ReadInput never reads
    an output-channel list, so this converter emits none either."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# IceDyn primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    #-------------------- structure_properties ------------------------------------
    n_legs = int(d.scalar('NumLegs'))
    leg_pos_x = d.find('LegPosX')[:n_legs]
    leg_pos_y = d.find('LegPosY')[:n_legs]
    str_wd    = d.find('StWidth')[:n_legs]
    w('structure_properties:')
    w('  LegPosX: [' + _list_join(leg_pos_x) + ']')
    w('  LegPosY: [' + _list_join(leg_pos_y) + ']')
    w('  StWidth: [' + _list_join(str_wd)    + ']')
    w('')

    #-------------------- ice_models -----------------------------------------------
    w('ice_models:')
    w('  IceModel: '    + d.scalar('IceModel'))
    w('  IceSubModel: ' + d.scalar('IceSubModel'))
    w('')

    #-------------------- ice_general -----------------------------------------------
    w('ice_general:')
    w('  IceVel: '  + d.scalar('IceVel'))
    w('  IceThks: ' + d.scalar('IceThks'))
    w('  WtDen: '   + d.scalar('WtDen'))
    w('  IceDen: '  + d.scalar('IceDen'))
    w('  InitLoc: ' + d.scalar('InitLoc'))
    w('  InitTm: '  + d.scalar('InitTm'))
    w('  Seed1: '   + d.scalar('Seed1'))
    w('  Seed2: '   + d.scalar('Seed2'))
    w('')

    #-------------------- ice_model_1 -----------------------------------------------
    w('ice_model_1:')
    w('  Ikm: '     + d.scalar('Ikm'))
    w('  Ag: '      + d.scalar('Ag'))
    w('  Qg: '      + d.scalar('Qg'))
    w('  Rg: '      + d.scalar('Rg'))
    w('  Tice: '    + d.scalar('Tice'))
    w('  Poisson: ' + d.scalar('Poisson'))
    w('  WgAngle: ' + d.scalar('WgAngle'))
    w('  EIce: '    + d.scalar('EIce'))
    w('  SigNm: '   + d.scalar('SigNm'))
    w('')

    #-------------------- ice_model_2 -----------------------------------------------
    w('ice_model_2:')
    w('  Pitch: '   + d.scalar('Pitch'))
    w('  IceStr2: ' + d.scalar('IceStr2'))
    w('  Delmax2: ' + d.scalar('Delmax2'))
    w('')

    #-------------------- ice_model_3 -----------------------------------------------
    w('ice_model_3:')
    w('  ThkMean: ' + d.scalar('ThkMean'))
    w('  ThkVar: '  + d.scalar('ThkVar'))
    w('  VelMean: ' + d.scalar('VelMean'))
    w('  VelVar: '  + d.scalar('VelVar'))
    w('  TeMean: '  + d.scalar('TeMean'))
    w('  StrMean: ' + d.scalar('StrMean'))
    w('  StrVar: '  + d.scalar('StrVar'))
    w('  DelMean: ' + d.scalar('DelMean'))
    w('  DelVar: '  + d.scalar('DelVar'))
    w('  PMean: '   + d.scalar('PMean'))
    w('  PVar: '    + d.scalar('PVar'))
    w('')

    #-------------------- ice_model_4 -----------------------------------------------
    w('ice_model_4:')
    w('  PrflMean: '  + d.scalar('PrflMean'))
    w('  PrflSig: '   + d.scalar('PrflSig'))
    w('  ZoneNo1: '   + d.scalar('ZoneNo1'))
    w('  ZoneNo2: '   + d.scalar('ZoneNo2'))
    w('  ZonePitch: ' + d.scalar('ZonePitch'))
    w('  IceStr: '    + d.scalar('IceStr'))
    w('  Delmax: '    + d.scalar('Delmax'))
    w('')

    #-------------------- ice_model_5 -----------------------------------------------
    w('ice_model_5:')
    w('  ConeAgl: '  + d.scalar('ConeAgl'))
    w('  ConeDwl: '  + d.scalar('ConeDwl'))
    w('  ConeDtp: '  + d.scalar('ConeDtp'))
    w('  RdupThk: '  + d.scalar('RdupThk'))
    w('  mu: '       + d.scalar('mu'))
    w('  FlxStr: '   + d.scalar('FlxStr'))
    w('  StrLim: '   + d.scalar('StrLim'))
    w('  StrRtLim: ' + d.scalar('StrRtLim'))
    w('')

    #-------------------- ice_model_6 -----------------------------------------------
    w('ice_model_6:')
    w('  FloeLth: ' + d.scalar('FloeLth'))
    w('  FloeWth: ' + d.scalar('FloeWth'))
    w('  CPrAr: '   + d.scalar('CPrAr'))
    w('  dPrAr: '   + d.scalar('dPrAr'))
    w('  Fdr: '     + d.scalar('Fdr'))
    w('  Kic: '     + d.scalar('Kic'))
    w('  FspN: '    + d.scalar('FspN'))

    return '\n'.join(out)


def convert_orca(text_path):
    """Convert a text-format OrcaFlex Interface primary input file to its YAML schema
    (modules/orcaflex-interface/src/Orca_Yaml.f90 is the source of truth).

    The text primary file is tiny: it reads only Echo, DirRoot (the OrcaFlex simulation
    input file), and DLL_FileName (the OrcaFlex DLL) -- the DT and OutList reads in
    ReadPrimaryFile are commented out and never exercised, so this converter emits
    neither. DirRoot/DLL_FileName are emitted VERBATIM (raw, unresolved paths): the
    Orca_Yaml.f90 reader stores them as-is and the ReadPrimaryFile funnel resolves
    relative paths against PriPath once, after either format's read completes.

    No convert_fst wiring: OrcaFlex has no reg-test deck (CompMooring=4 requires the
    proprietary OrcaFlex DLL, unavailable in CI), so this converter is defined for
    completeness only and is not invoked by convert_fst's single-file assembly."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# OrcaFlex Interface primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    #-------------------- simulation_control -------------------------------------
    w('simulation_control:')
    w('  Echo: '         + _as_bool(d.scalar('Echo')))
    w('  DirRoot: '      + _as_str(d.scalar('DirRoot')))
    w('  DLL_FileName: ' + _as_str(d.scalar('DLL_FileName')))

    return '\n'.join(out)


def convert_icefloe(text_path):
    """Convert a text-format IceFloe primary input file to its YAML schema
    (modules/icefloe/src/interfaces/FAST/IceFloe_Yaml.f90 is the source of truth).

    Unlike the other converters, IceFloe's text format has no field-by-field schema: it's a
    flat list of "NAME value" lines (comment lines start with '!', '#', '$', or '%', per
    modules/icefloe/src/icefloe/iceInput.f90's countIceInputs/readIceInputs). So this
    converter does not know IceFloe's parameter names either -- it emits one top-level
    "NAME: value" mapping entry per non-comment line, in file order, preserving the numeric
    literal verbatim (no reformatting) so IceFloe_ParseYamlInputs's real(ReKi) read sees the
    same text the text-format reader would have. IceFloe has no standalone reg-test driver
    entry in convert_fst (IceFile stays a plain text path there -- inline glue is out of
    scope, see the CompIce==2 comment above), so this function is defined for direct/future
    use but is not wired into convert_fst's dispatch."""
    out = []
    w = out.append
    w('# IceFloe primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    with open(text_path, 'r') as f:
        for line in f:
            stripped = line.strip()
            if not stripped:
                continue
            if stripped[0] in '!#$%':
                continue
            parts = stripped.split(None, 1)
            if len(parts) < 2:
                continue
            name, rest = parts[0], parts[1]
            # keep only the value token (drop any trailing inline comment/description text)
            value = rest.split(None, 1)[0]
            w('{}: {}'.format(name, value))

    return '\n'.join(out)


def convert_beamdyn(text_path):
    """Convert a text-format BeamDyn primary input file to its YAML schema.

    Sections mirror the text file's banners (simulation_control, geometry_parameter,
    mesh_parameter, beam_sectional_parameter, outputs, nodal_outputs).
    member_total/kp_total are NOT emitted (list lengths derive them in
    BeamDyn_Yaml.f90): geometry_parameter:kp_member is the ordered per-member key-point
    count list (the text format's leading "member number" column is dropped -- it is
    only ever used by the text reader to warn about out-of-order entry, and a YAML list
    is unambiguously ordered), and geometry_parameter:key_points is the kp_total x 4
    (x, y, z, twist) table. BldFile stays a path string (a second-order file: never
    converted/inlined). NNodeOuts derives from outputs:OutNd's list length. NumOuts and
    nodal_outputs' BldNd_NumOuts derive from their respective OutList lengths.
    refine/n_fact/DTBeam/load_retries/NRMax/stop_tol/tngt_stf_fd/tngt_stf_comp/
    tngt_stf_pert/tngt_stf_difftol all accept the literal scalar "default"/"DEFAULT"."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# BeamDyn primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('simulation_control:')
    w('  Echo: '             + _as_bool(d.scalar('Echo')))
    w('  QuasiStaticInit: '  + _as_bool(d.scalar('QuasiStaticInit')))
    w('  rhoinf: '           + d.scalar('rhoinf'))
    w('  quadrature: '       + d.scalar('quadrature'))
    w('  refine: '           + _default_or_num(d.scalar('refine')))
    w('  n_fact: '           + _default_or_num(d.scalar('n_fact')))
    w('  DTBeam: '           + _default_or_num(d.scalar('DTBeam')))
    w('  load_retries: '     + _default_or_num(d.scalar('load_retries')))
    w('  NRMax: '            + _default_or_num(d.scalar('NRMax')))
    w('  stop_tol: '         + _default_or_num(d.scalar('stop_tol')))
    w('  tngt_stf_fd: '      + _default_or_bool(d.scalar('tngt_stf_fd')))
    w('  tngt_stf_comp: '    + _default_or_bool(d.scalar('tngt_stf_comp')))
    w('  tngt_stf_pert: '    + _default_or_num(d.scalar('tngt_stf_pert')))
    w('  tngt_stf_difftol: '+ _default_or_num(d.scalar('tngt_stf_difftol')))
    w('  RotStates: '        + _as_bool(d.scalar('RotStates')))
    w('')

    w('geometry_parameter:')
    member_total = int(d.scalar('member_total'))
    kp_total     = int(d.scalar('kp_total'))
    # member table: "member_number  kp_count_in_member" pairs, no header line; only the
    # count (2nd token) survives to YAML (see the docstring above)
    member_rows = _matrix_rows(d, member_total, 2, skip=0)
    kp_member = [row[1] for row in member_rows]
    w('  kp_member: [' + ', '.join(kp_member) + ']')
    # key-point table: two descriptive header lines, then kp_total rows of x,y,z,twist
    kp_rows = _matrix_rows(d, kp_total, 4, skip=2)
    w('  key_points:')
    for row in kp_rows:
        w('    - [' + ', '.join(row) + ']')
    w('')

    w('mesh_parameter:')
    w('  order_elem: ' + d.scalar('order_elem'))
    w('')

    w('beam_sectional_parameter:')
    w('  BldFile: ' + _as_str(d.scalar('BldFile')))
    w('')

    w('outputs:')
    w('  SumPrint: ' + _as_bool(d.scalar('SumPrint')))
    w('  OutFmt: '   + _as_str(d.scalar('OutFmt')))
    n_node_outs = int(d.scalar('NNodeOuts'))
    outnd = d.find('OutNd')[:n_node_outs]
    w('  OutNd: [' + _list_join(outnd) + ']')
    channels = d.outlist()
    w('  OutList: [' + ', '.join('"' + c + '"' for c in channels) + ']')
    w('')

    # nodal_outputs is optional -- some r-test decks (e.g. glue-code BDBldFile targets)
    # omit the trailing "OutList for Blade node channels" section entirely, matching
    # the text reader's own tolerate-a-missing-section fallback (BD_ReadPrimaryFile
    # just leaves BldNd_NumOuts = 0 rather than erroring); only emit the section here
    # when the keyword is actually present in the remaining, unconsumed text
    remaining = '\n'.join(d.lines[d.cursor:])
    if re.search(r'(?i)(?<![A-Za-z0-9_])BldNd_BlOutNd(?![A-Za-z0-9_])', remaining):
        w('nodal_outputs:')
        w('  BldNd_BlOutNd: ' + _as_str(d.scalar('BldNd_BlOutNd')))
        nd_channels = d.outlist()
        w('  OutList: [' + ', '.join('"' + c + '"' for c in nd_channels) + ']')
        w('')

    return '\n'.join(out)


def convert_beamdyn_driver(text_path):
    """Convert a text-format BeamDyn driver input file (Wave 4, class-B/sequential-
    reader driver, with a multi-point-loads table -- follows the subdyn convention) to
    its YAML schema (modules/beamdyn/src/BeamDyn_Driver_Yaml.f90 is the source of
    truth).

    Schema mirrors the driver's text reader (BD_ReadDvrFile, Driver_Beam_Subs.f90:72-233)
    key-for-key:
        simulation_control:      DynamicSolve, t_initial, t_final, dt
        gravity_parameter:       gravity (list of 3: Gx, Gy, Gz)
        frame_parameter:         GlbPos (list of 3), RootOri (3x3 direction-cosine
                                  matrix, one flow-sequence row per line),
                                  GlbRotBladeT0
        root_velocity_parameter: RootVel (list of 3: RootVel(4)/(5)/(6), the angular
                                  velocity components -- RootVel(1:3) is a derived
                                  cross-product, never read from the file in either
                                  format)
        applied_force:           DistrLoad (list of 6), TipLoad (list of 6)
        multi_point_loads:       point_loads (list of row mappings, empty list when
                                  NumPointLoads == 0; NumPointLoads itself derives from
                                  the list length)
        primary_input_file:      InputFile
        outputs:                 WrVTK, VTK_fps

    The driver's text reader has no Echo option (unlike BeamDyn's own primary input
    file reader), so this schema has no general:Echo section either -- a true
    key-for-key match, not an omission. InputFile stays path-valued (second-order
    rule; not repointed at a .yaml sibling here -- that repointing, when needed, is
    done by the caller after this converter returns, mirroring subdyn's driver-mode
    harness convention)."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# BeamDyn driver input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('simulation_control:')
    w('  DynamicSolve: ' + _as_bool(d.scalar('DynamicSolve')))
    w('  t_initial: ' + d.scalar('t_initial'))
    w('  t_final: ' + d.scalar('t_final'))
    w('  dt: ' + d.scalar('dt'))
    w('')

    w('gravity_parameter:')
    gravity = [d.scalar('Gx'), d.scalar('Gy'), d.scalar('Gz')]
    w('  gravity: [' + ', '.join(gravity) + ']')
    w('')

    w('frame_parameter:')
    glb_pos = [d.scalar('GlbPos(1)'), d.scalar('GlbPos(2)'), d.scalar('GlbPos(3)')]
    w('  GlbPos: [' + ', '.join(glb_pos) + ']')
    # the direction cosine matrix: two banner lines (not comments -- no !#% prefix),
    # then 3 rows of 3 numbers each
    root_ori_rows = _matrix_rows(d, 3, 3, skip=2)
    w('  RootOri:')
    for row in root_ori_rows:
        w('    - [' + ', '.join(row) + ']')
    w('  GlbRotBladeT0: ' + _as_bool(d.scalar('GlbRotBladeT0')))
    w('')

    w('root_velocity_parameter:')
    root_vel = [d.scalar('RootVel(4)'), d.scalar('RootVel(5)'), d.scalar('RootVel(6)')]
    w('  RootVel: [' + ', '.join(root_vel) + ']')
    w('')

    w('applied_force:')
    distr_load = [d.scalar('DistrLoad({})'.format(k)) for k in range(1, 7)]
    w('  DistrLoad: [' + ', '.join(distr_load) + ']')
    tip_load = [d.scalar('TipLoad({})'.format(k)) for k in range(1, 7)]
    w('  TipLoad: [' + ', '.join(tip_load) + ']')
    w('')

    # multi-point loads: NumPointLoads header, 2 descriptive header lines (not
    # comments), then NumPointLoads rows of 7 values (Eta, Fx, Fy, Fz, Mx, My, Mz)
    n_loads = int(d.scalar('NumPointLoads'))
    load_rows = _matrix_rows(d, n_loads, 7, skip=2)
    POINT_LOAD_KEYS = ('Eta', 'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz')
    w('multi_point_loads:')
    if n_loads > 0:
        w('  point_loads:')
        for row in load_rows:
            w(_row_map(POINT_LOAD_KEYS, row))
    else:
        w('  point_loads: []')
    w('')

    w('primary_input_file:')
    w('  InputFile: ' + _as_str(d.scalar('InputFile')))
    w('')

    w('outputs:')
    w('  WrVTK: ' + d.scalar('WrVTK'))
    w('  VTK_fps: ' + d.scalar('VTK_fps'))
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


def convert_seastate_driver(text_path):
    """Convert a text-format SeaState driver input file (Wave 4, class-B/sequential-
    reader driver -- unlike aerodisk/sed's class-A FileInfoType/ParseVar drivers, but
    the YAML schema convention is unaffected: this converter is dependency-free the
    same way) to its YAML schema.

    Schema mirrors the driver's text reader (SeaState_DriverCode.f90:309-605,
    ReadDriverInputFile) key-for-key:
        general:                   Echo
        environmental_conditions:  Gravity, WtrDens, WtrDpth, MSL2SWL
        seastate:                  SeaStateInputFile, OutRootName, WrWvKinMod, NSteps, TimeInterval
        wave_elevation_series:     WaveElevVis

    Every field here is a plain ReadVar read in the text path (no "default" keyword
    anywhere, unlike the SeaState *primary* file's WtrDens/WtrDpth/MSL2SWL) -- verbatim
    numeric literals throughout, no _default_or_num. WaveElevVisNx/WaveElevVisNy are
    declared in the driver's InitInp type but are never read by the text path either
    (a commented-out FIXME in ReadDriverInputFile) -- not emitted here either.

    SeaStateInputFile is repointed at the YAML primary (any original extension --
    .dat/.inp/... -- becomes .yaml, matching the executeYamlEquivalenceCase.py seastate
    block's own unconditional splitext+".yaml" convention for the primary file, since
    r-test SeaState primary filenames vary per case and are not always ".inp") so a
    driver-mode yaml-equivalence case (yaml driver -> yaml primary) is fully YAML end
    to end.
    """
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# SeaState driver input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('')

    w('environmental_conditions:')
    w('  Gravity: ' + d.scalar('Gravity'))
    w('  WtrDens: ' + d.scalar('WtrDens'))
    w('  WtrDpth: ' + d.scalar('WtrDpth'))
    w('  MSL2SWL: ' + d.scalar('MSL2SWL'))
    w('')

    w('seastate:')
    seastate_ipt = _unquote(d.scalar('SeaStateInputFile'))
    base, ext = os.path.splitext(seastate_ipt)
    seastate_ipt = base + '.yaml'
    w('  SeaStateInputFile: ' + _as_str(seastate_ipt))
    w('  OutRootName: ' + _as_str(d.scalar('OutRootName')))
    w('  WrWvKinMod: ' + d.scalar('WrWvKinMod'))
    w('  NSteps: ' + d.scalar('NSteps'))
    w('  TimeInterval: ' + d.scalar('TimeInterval'))
    w('')

    w('wave_elevation_series:')
    w('  WaveElevVis: ' + _as_bool(d.scalar('WaveElevSeriesFlag')))
    w('')

    return '\n'.join(out)


def _file_list_tokens(toks, n):
    """Return the first `n` path tokens from a text-format file-list line, stripping
    separator commas (both freestanding "," tokens and trailing commas on tokens)."""
    cleaned = [t.rstrip(',') for t in toks if t.rstrip(',')]
    if len(cleaned) < n:
        raise ValueError('file list has {} entr(ies); expected at least {}'.format(len(cleaned), n))
    return cleaned[:n]


def convert_servodyn(text_path, stc_to_yaml=False):
    """Convert a text-format ServoDyn primary input file to its YAML schema.

    Sections mirror the text file's banners (general, pitch_control,
    generator_torque_control, simple_variable_speed, simple_induction_generator,
    thevenin_generator, high_speed_shaft_brake, yaw_control, aero_flow_control,
    structural_control, cable_control, bladed_interface, torque_speed_lookup, output).
    DT and DLL_DT accept the scalar "default" (ParseVarWDefault in the text path).
    Per-blade values (PitNeut, PitSpr, PitDamp, TPitManS, PitManRat, BlPitchF) are
    read as three separate keyword lines in the text format and become 3-entry lists.
    NumBStC/NumNStC/NumTStC/NumSStC and DLL_NumTrq and NumOuts are derived in YAML from
    list lengths, so the text-format counts are consumed here only to know how many
    entries to slice, and are never themselves emitted. Angle/speed values are copied
    verbatim in their native units (deg / rpm / %); ServoDyn_Yaml.f90 applies the same
    conversions as the text path right after reading them.

    stc_to_yaml: when True, every referenced StC sub-file (BStCfiles/NStCfiles/
    TStCfiles/SStCfiles) is ALSO converted to YAML (convert_stc) and the list entries
    are repointed at the new .yaml names; the converted documents are returned in
    extra_files (path relative to the ServoDyn deck -> yaml text). StC files are a
    separate file type, always referenced by path -- never inlined.

    Returns (yaml_text, extra_files) -- the same tuple-return shape as convert_fst's own
    sibling-extra-files precedent (used there for all-yaml mode's converted module
    files, mirrored here one level down for StC sub-files)."""
    d = _TextDeck(text_path)
    extra_files = {}

    out = []
    w = out.append
    w('# ServoDyn primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('  DT: ' + _default_or_num(d.scalar('DT')))
    w('')

    w('pitch_control:')
    w('  PCMode: ' + d.scalar('PCMode'))
    w('  TPCOn: '  + d.scalar('TPCOn'))
    for key in ('PitNeut', 'PitSpr', 'PitDamp', 'TPitManS', 'PitManRat', 'BlPitchF'):
        vals = [d.scalar('{}({})'.format(key, k)) for k in (1, 2, 3)]
        w('  {}: [{}]'.format(key, _list_join(vals)))
    w('')

    w('generator_torque_control:')
    w('  VSContrl: ' + d.scalar('VSContrl'))
    w('  GenModel: ' + d.scalar('GenModel'))
    w('  GenEff: '   + d.scalar('GenEff'))
    w('  GenTiStr: ' + _as_bool(d.scalar('GenTiStr')))
    w('  GenTiStp: ' + _as_bool(d.scalar('GenTiStp')))
    w('  SpdGenOn: ' + d.scalar('SpdGenOn'))
    w('  TimGenOn: ' + d.scalar('TimGenOn'))
    w('  TimGenOf: ' + d.scalar('TimGenOf'))
    w('')

    w('simple_variable_speed:')
    w('  VS_RtGnSp: ' + d.scalar('VS_RtGnSp'))
    w('  VS_RtTq: '   + d.scalar('VS_RtTq'))
    w('  VS_Rgn2K: '  + d.scalar('VS_Rgn2K'))
    w('  VS_SlPc: '   + d.scalar('VS_SlPc'))
    w('')

    w('simple_induction_generator:')
    w('  SIG_SlPc: ' + d.scalar('SIG_SlPc'))
    w('  SIG_SySp: ' + d.scalar('SIG_SySp'))
    w('  SIG_RtTq: ' + d.scalar('SIG_RtTq'))
    w('  SIG_PORt: ' + d.scalar('SIG_PORt'))
    w('')

    w('thevenin_generator:')
    for key in ('TEC_Freq', 'TEC_NPol', 'TEC_SRes', 'TEC_RRes', 'TEC_VLL', 'TEC_SLR', 'TEC_RLR', 'TEC_MR'):
        w('  {}: {}'.format(key, d.scalar(key)))
    w('')

    w('high_speed_shaft_brake:')
    w('  HSSBrMode: ' + d.scalar('HSSBrMode'))
    w('  THSSBrDp: '  + d.scalar('THSSBrDp'))
    w('  HSSBrDT: '   + d.scalar('HSSBrDT'))
    w('  HSSBrTqF: '  + d.scalar('HSSBrTqF'))
    w('')

    w('yaw_control:')
    w('  YCMode: '    + d.scalar('YCMode'))
    w('  TYCOn: '     + d.scalar('TYCOn'))
    w('  YawNeut: '   + d.scalar('YawNeut'))
    w('  YawSpr: '    + d.scalar('YawSpr'))
    w('  YawDamp: '   + d.scalar('YawDamp'))
    w('  TYawManS: '  + d.scalar('TYawManS'))
    w('  YawManRat: ' + d.scalar('YawManRat'))
    w('  NacYawF: '   + d.scalar('NacYawF'))
    w('')

    w('aero_flow_control:')
    w('  AfCmode: '   + d.scalar('AfCmode'))
    w('  AfC_Mean: '  + d.scalar('AfC_Mean'))
    w('  AfC_Amp: '   + d.scalar('AfC_Amp'))
    w('  AfC_Phase: ' + d.scalar('AfC_Phase'))
    w('')

    w('structural_control:')
    for count_key, list_key in (('NumBStC', 'BStCfiles'), ('NumNStC', 'NStCfiles'),
                                ('NumTStC', 'TStCfiles'), ('NumSStC', 'SStCfiles')):
        n = int(d.scalar(count_key))
        files = _file_list_tokens(d.find(list_key), n)
        names = [_unquote(t) for t in files]
        if stc_to_yaml:
            for i, name in enumerate(names):
                yaml_rel = os.path.splitext(name)[0] + '.yaml'
                if yaml_rel not in extra_files:
                    extra_files[yaml_rel] = convert_stc(os.path.join(d.base_dir, name))
                names[i] = yaml_rel
        w('  {}: [{}]'.format(list_key, ', '.join(_as_str(n2) for n2 in names)))
    w('')

    w('cable_control:')
    w('  CCmode: ' + d.scalar('CCmode'))
    w('')

    w('bladed_interface:')
    w('  DLL_FileName: ' + _as_str(d.scalar('DLL_FileName')))
    w('  DLL_InFile: '   + _as_str(d.scalar('DLL_InFile')))
    w('  DLL_ProcName: ' + _as_str(d.scalar('DLL_ProcName')))
    w('  DLL_DT: '       + _default_or_num(d.scalar('DLL_DT')))
    w('  DLL_Ramp: '     + _as_bool(d.scalar('DLL_Ramp')))
    w('  BPCutoff: '     + d.scalar('BPCutoff'))
    w('  NacYaw_North: ' + d.scalar('NacYaw_North'))
    w('  Ptch_Cntrl: '   + d.scalar('Ptch_Cntrl'))
    w('  Ptch_SetPnt: '  + d.scalar('Ptch_SetPnt'))
    w('  Ptch_Min: '     + d.scalar('Ptch_Min'))
    w('  Ptch_Max: '     + d.scalar('Ptch_Max'))
    w('  PtchRate_Min: ' + d.scalar('PtchRate_Min'))
    w('  PtchRate_Max: ' + d.scalar('PtchRate_Max'))
    w('  Gain_OM: '      + d.scalar('Gain_OM'))
    w('  GenSpd_MinOM: ' + d.scalar('GenSpd_MinOM'))
    w('  GenSpd_MaxOM: ' + d.scalar('GenSpd_MaxOM'))
    w('  GenSpd_Dem: '   + d.scalar('GenSpd_Dem'))
    w('  GenTrq_Dem: '   + d.scalar('GenTrq_Dem'))
    w('  GenPwr_Dem: '   + d.scalar('GenPwr_Dem'))
    w('')

    # torque-speed look-up table: DLL_NumTrq is derived in YAML from the (equal)
    # lengths of the GenSpd_TLU/GenTrq_TLU lists. The text format's two descriptive
    # header/units lines ("GenSpd_TLU GenTrq_TLU" / "(rpm) (Nm)") are always present
    # and skipped without parsing, exactly as ServoDyn_IO.f90 does (CurLine += 1 twice).
    n_trq = int(d.scalar('DLL_NumTrq'))
    genspd, gentrq = [], []
    if n_trq > 0:
        for row in d.table_rows(n_trq, skip=2):
            toks = [t for t in re.split(r'[,\s]+', row.strip()) if t]
            if len(toks) != 2:
                raise ValueError('convert_servodyn: torque-speed table row "{}" in {} has {} value(s); '
                                 'expected 2'.format(row, text_path, len(toks)))
            genspd.append(toks[0])
            gentrq.append(toks[1])
    w('torque_speed_lookup:')
    w('  GenSpd_TLU: [' + ', '.join(genspd) + ']')
    w('  GenTrq_TLU: [' + ', '.join(gentrq) + ']')
    w('')

    w('output:')
    w('  SumPrint: ' + _as_bool(d.scalar('SumPrint')))
    w('  OutFile: '  + d.scalar('OutFile'))
    w('  TabDelim: ' + _as_bool(d.scalar('TabDelim')))
    w('  OutFmt: '   + _as_str(d.scalar('OutFmt')))
    w('  TStart: '   + d.scalar('TStart'))
    channels = d.outlist()
    w('  OutList: [' + ', '.join('"' + c + '"' for c in channels) + ']')
    w('')

    return '\n'.join(out), extra_files


def convert_stc(text_path):
    """Convert a text-format Structural Control (StC) input file to its YAML schema.

    Sections mirror the text file's banners (general, degrees_of_freedom, location,
    initial_conditions, configuration, mass_stiffness_damping,
    user_defined_spring_forces, control, tlcd, prescribed_time_series).
    StC_Z_PreLd is a variant string field ("gravity", "none", or a number) copied
    through verbatim as a string. NKInpSt is derived in YAML from the number of rows
    of the F_TBL matrix; the text-format count is consumed here only to know how many
    rows to read. StC_CChan and PrescribedForcesFile may each be a single value or a
    per-instance list in the text format (the reader tries a full array first, then
    falls back to one value broadcast) -- they are emitted as a scalar when one value
    is present and a list otherwise, and StrucCtrl_Yaml.f90 accepts both forms."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# Structural Control (StC) input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('')

    w('degrees_of_freedom:')
    w('  StC_DOF_MODE: ' + d.scalar('StC_DOF_MODE'))
    w('  StC_X_DOF: ' + _as_bool(d.scalar('StC_X_DOF')))
    w('  StC_Y_DOF: ' + _as_bool(d.scalar('StC_Y_DOF')))
    w('  StC_Z_DOF: ' + _as_bool(d.scalar('StC_Z_DOF')))
    w('')

    w('location:')
    w('  StC_P_X: ' + d.scalar('StC_P_X'))
    w('  StC_P_Y: ' + d.scalar('StC_P_Y'))
    w('  StC_P_Z: ' + d.scalar('StC_P_Z'))
    w('')

    w('initial_conditions:')
    w('  StC_X_DSP: ' + d.scalar('StC_X_DSP'))
    w('  StC_Y_DSP: ' + d.scalar('StC_Y_DSP'))
    w('  StC_Z_DSP: ' + d.scalar('StC_Z_DSP'))
    w('  StC_Z_PreLd: ' + _as_str(d.scalar('StC_Z_PreLd')))
    w('')

    w('configuration:')
    for key in ('StC_X_PSP', 'StC_X_NSP', 'StC_Y_PSP', 'StC_Y_NSP', 'StC_Z_PSP', 'StC_Z_NSP'):
        w('  {}: {}'.format(key, d.scalar(key)))
    w('')

    w('mass_stiffness_damping:')
    for key in ('StC_X_M', 'StC_Y_M', 'StC_Z_M', 'StC_Omni_M',
                'StC_X_K', 'StC_Y_K', 'StC_Z_K',
                'StC_X_C', 'StC_Y_C', 'StC_Z_C',
                'StC_X_KS', 'StC_Y_KS', 'StC_Z_KS',
                'StC_X_CS', 'StC_Y_CS', 'StC_Z_CS'):
        w('  {}: {}'.format(key, d.scalar(key)))
    w('')

    # NKInpSt is derived in YAML from the F_TBL row count. The text format's three
    # descriptive lines after NKInpSt (the "StC SPRING FORCES TABLE" section banner,
    # the column-header line, and the units line) are always present and skipped
    # without parsing, exactly as StrucCtrl.f90 does (CurLine += 1 three times).
    w('user_defined_spring_forces:')
    w('  Use_F_TBL: ' + _as_bool(d.scalar('Use_F_TBL')))
    n_k = int(d.scalar('NKInpSt'))
    if n_k > 0:
        w('  F_TBL:')
        for row in d.table_rows(n_k, skip=3):
            toks = [t for t in re.split(r'[,\s]+', row.strip()) if t]
            if len(toks) != 6:
                raise ValueError('convert_stc: spring-force table row "{}" in {} has {} value(s); '
                                 'expected 6'.format(row, text_path, len(toks)))
            w('    - [' + ', '.join(toks) + ']')
    else:
        w('  F_TBL: []')
    w('')

    w('control:')
    w('  StC_CMODE: ' + d.scalar('StC_CMODE'))
    # StC_CChan: the text reader tries a NumMeshPts-long array first, then falls back
    # to a single value broadcast to every instance. Emit whatever the line holds:
    # one value -> scalar, several -> list (StrucCtrl_Yaml.f90 accepts both forms).
    cchan = [t.rstrip(',') for t in d.find('StC_CChan') if t.rstrip(',')]
    if len(cchan) == 1:
        w('  StC_CChan: ' + cchan[0])
    else:
        w('  StC_CChan: [' + ', '.join(cchan) + ']')
    for key in ('StC_SA_MODE', 'StC_X_C_HIGH', 'StC_X_C_LOW', 'StC_Y_C_HIGH', 'StC_Y_C_LOW',
                'StC_Z_C_HIGH', 'StC_Z_C_LOW', 'StC_X_C_BRAKE', 'StC_Y_C_BRAKE', 'StC_Z_C_BRAKE'):
        w('  {}: {}'.format(key, d.scalar(key)))
    w('')

    w('tlcd:')
    for key in ('L_X', 'B_X', 'area_X', 'area_ratio_X', 'headLossCoeff_X', 'rho_X',
                'L_Y', 'B_Y', 'area_Y', 'area_ratio_Y', 'headLossCoeff_Y', 'rho_Y'):
        w('  {}: {}'.format(key, d.scalar(key)))
    w('')

    w('prescribed_time_series:')
    # the keyword is written truncated ("PrescribedForcesCoord-") in most distributed
    # StC files; accept both spellings
    try:
        w('  PrescribedForcesCoordSys: ' + d.scalar('PrescribedForcesCoordSys'))
    except KeyError:
        w('  PrescribedForcesCoordSys: ' + d.scalar('PrescribedForcesCoord'))
    # PrescribedForcesFile: one line per instance in the text format (blade StCs may
    # list one file per blade; missing lines fall back to the first file). Collect
    # every consecutive PrescribedForcesFile line: one -> scalar, several -> list.
    frc_files = [_as_str(d.scalar('PrescribedForcesFile'))]
    while d.cursor < len(d.lines):
        toks = _tokens_before_keyword(d.lines[d.cursor], 'PrescribedForcesFile')
        if toks is None or not toks:
            break
        frc_files.append(_as_str(toks[0]))
        d.cursor += 1
    if len(frc_files) == 1:
        w('  PrescribedForcesFile: ' + frc_files[0])
    else:
        w('  PrescribedForcesFile: [' + ', '.join(frc_files) + ']')
    w('')

    return '\n'.join(out)


def _row_tokens(raw):
    """Whitespace/comma-separated tokens of one already-comment-stripped table row."""
    return [t for t in re.split(r'[,\s]+', raw.strip()) if t]


def _matrix_rows(d, nrows, ncols, skip=0):
    """Read `nrows` text rows and slice each to its first `ncols` tokens.

    HydroDyn's AddF0/AddCLin/AddBLin/AddBQuad rows carry a descriptive keyword label
    ("AddF0", "AddCLin", ...) as trailing text on their first row only (not a comment
    -- there is no comment character before it); HydroDyn_Input.f90's ParseAry-based
    reads simply stop once they have consumed the expected number of values, so any
    trailing label text is never read. Slicing to `ncols` here reproduces that."""
    rows = []
    for raw in d.table_rows(nrows, skip=skip):
        toks = _row_tokens(raw)
        if len(toks) < ncols:
            raise ValueError('_matrix_rows: row "{}" has {} value(s); expected at least {}'.format(
                raw, len(toks), ncols))
        rows.append(toks[:ncols])
    return rows


def _bare_lines(d, n):
    """Return the next `n` non-comment/blank lines' first value token each -- bare,
    unlabeled continuation entries (AeroDyn's AFNames/ADBlFile lines after the first,
    read via ParseVar with an empty keyname, so there is no keyword to search for)."""
    vals = []
    while len(vals) < n:
        if d.cursor >= len(d.lines):
            raise ValueError('_bare_lines: ran out of lines in {} before collecting {} entr(ies)'.format(
                d.path, n))
        raw = d.lines[d.cursor]
        d.cursor += 1
        if _is_comment_or_blank(raw):
            continue
        stripped = _strip_inline_comment(raw).strip()
        m = re.match(r'"[^"]*"|\'[^\']*\'|\S+', stripped)
        if m is None:
            continue
        vals.append(m.group(0))
    return vals


def _ed_num_blades(text_path):
    """Read NumBl (blade count) from an ElastoDyn or Simplified ElastoDyn primary file --
    used by convert_fst to tell convert_aerodyn the true per-rotor blade count (AeroDyn's
    own primary file never carries it; the Fortran glue code supplies it from ElastoDyn's
    own initialization output)."""
    return int(_TextDeck(text_path).scalar('NumBl'))


def _row_map(keys, toks, indent='    '):
    """One YAML block-mapping table row ("- Key1: val1\\n  Key2: val2\\n..."), using
    only as many of `keys` as `toks` supplies (fewer toks than keys means the trailing
    keys are omitted -- HydroDyn_Yaml.f90 applies the same fallback defaults for those
    as the text path's variant-length reads).

    A flow mapping ("- {k: v, ...}") would be more compact, but YamlInput.f90's
    block-sequence-item reader only special-cases a bare scalar or a single "key:
    value" line for a one-line item; a "{...}" item falls through to being split on
    its first colon as if it were itself "key: value", which is wrong. Multi-line
    block-mapping items (one key per line, all sharing the same indent) are the
    reliably-parsed form, so every table row here uses that instead."""
    pairs = list(zip(keys, toks))
    lines = ['{}- {}: {}'.format(indent, pairs[0][0], pairs[0][1])]
    for k, t in pairs[1:]:
        lines.append('{}  {}: {}'.format(indent, k, t))
    return '\n'.join(lines)


def convert_hydrodyn(text_path):
    """Convert a text-format HydroDyn primary input file to its YAML schema
    (modules/hydrodyn/src/HydroDyn_Yaml.f90 is the source of truth).

    Sections mirror the text file's banners (general, floating_platform,
    second_order_wamit_forces, additional_stiffness_damping, strip_theory,
    axial_coefficients, member_joints, cylindrical_member_cross_section,
    rectangular_member_cross_section, simple_hydrodynamic_coefficients_cylindrical,
    simple_hydrodynamic_coefficients_rectangular,
    depth_based_hydrodynamic_coefficients_cylindrical/rectangular,
    member_based_hydrodynamic_coefficients_cylindrical/rectangular, members,
    filled_members, marine_growth_by_depth, member_output_list, joint_output_list,
    output, output_channels).

    Every counted table (axial_coefficients:AxialCoefs, member_joints:Joints,
    {cylindrical,rectangular}_member_cross_section:MPropSets{Cyl,Rec},
    {depth_based,member_based}_hydrodynamic_coefficients_*:Coef*, members:Members,
    filled_members:FilledGroups, marine_growth_by_depth:MGDepths,
    member_output_list:MOutLst) becomes a YAML list of flow mappings, one per row,
    keyed by the documented column names -- so the N* counts (NAxCoef, NJoints, ...)
    are consumed here only to know how many rows to read, and are never themselves
    emitted (HydroDyn_Yaml.f90 derives them back from the list lengths).
    joint_output_list:JOutLst (a flat list of joint IDs) is the one counted table that
    stays a plain list, since it is not itself tabular.

    RdtnDT and each filled_members row's FillDens accept the literal scalar
    "default"/"DEFAULT" (ParseVarWDefault / a Conv2UC-then-compare in the text path).
    The MacCamy-Fuchs "MCF" keyword (ParseRAryWKywrd in the text path) is copied
    through verbatim wherever it appears (SimplCp/SimplCpMG, SimplRecCp/SimplRecCpMG,
    DpthCp/DpthCpMG, MemberCp1/MemberCp2/MemberCpMG1/MemberCpMG2) -- HydroDyn_Yaml.f90
    accepts either a number or literal "MCF" for those keys.

    additional_stiffness_damping:AddCLin/AddBLin/AddBQuad are 3-D in the text format
    (NDOF rows of nWAMITObj*NDOF flattened values -- nWAMITObj blocks of NDOF values
    concatenated); here they become a YAML list of nWAMITObj NDOF x NDOF matrices (one
    per WAMIT object), matching HydroDyn_Yaml.f90's per-object matrix reads."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# HydroDyn primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('')

    #-------------------- floating_platform -----------------------------------------
    w('floating_platform:')
    w('  PotMod: ' + d.scalar('PotMod'))
    w('  ExctnMod: ' + d.scalar('ExctnMod'))
    w('  ExctnDisp: ' + d.scalar('ExctnDisp'))
    w('  ExctnCutOff: ' + d.scalar('ExctnCutOff'))
    w('  PtfmYMod: ' + d.scalar('PtfmYMod'))
    w('  PtfmRefY: ' + d.scalar('PtfmRefY'))
    w('  PtfmYCutOff: ' + d.scalar('PtfmYCutOff'))
    w('  NExctnHdg: ' + d.scalar('NExctnHdg'))
    w('  RdtnMod: ' + d.scalar('RdtnMod'))
    w('  RdtnTMax: ' + d.scalar('RdtnTMax'))
    w('  RdtnDT: ' + _default_or_num(d.scalar('RdtnDT')))

    n_body = int(d.scalar('NBody'))
    n_body_mod = int(d.scalar('NBodyMod'))
    w('  NBody: ' + str(n_body))
    w('  NBodyMod: ' + str(n_body_mod))
    if n_body_mod == 1:
        n_wamit_obj = 1
        vec_multiplier = n_body
    else:
        n_wamit_obj = n_body
        vec_multiplier = 1

    pot_file_toks = _file_list_tokens(d.find('PotFile'), n_wamit_obj)
    w('  PotFile: [' + ', '.join(_as_str(t) for t in pot_file_toks) + ']')
    w('  WAMITULEN: [' + _list_join(d.find('WAMITULEN')[:n_wamit_obj]) + ']')
    w('  PtfmRefxt: [' + _list_join(d.find('PtfmRefxt')[:n_body]) + ']')
    w('  PtfmRefyt: [' + _list_join(d.find('PtfmRefyt')[:n_body]) + ']')
    w('  PtfmRefzt: [' + _list_join(d.find('PtfmRefzt')[:n_body]) + ']')
    w('  PtfmRefztRot: [' + _list_join(d.find('PtfmRefztRot')[:n_body]) + ']')
    w('  PtfmVol0: [' + _list_join(d.find('PtfmVol0')[:n_body]) + ']')
    w('  PtfmCOBxt: [' + _list_join(d.find('PtfmCOBxt')[:n_body]) + ']')
    w('  PtfmCOByt: [' + _list_join(d.find('PtfmCOByt')[:n_body]) + ']')
    naddDOF_toks = [t.rstrip(',') for t in d.find('NAddDOF')][:n_body]
    w('  NAddDOF: [' + ', '.join(naddDOF_toks) + ']')
    w('  FKMod: [' + _list_join(d.find('FKMod')[:n_wamit_obj]) + ']')
    geo_file_toks = _file_list_tokens(d.find('GeoFile'), n_body)
    w('  GeoFile: [' + ', '.join(_as_str(t) for t in geo_file_toks) + ']')
    w('')

    #-------------------- second_order_wamit_forces ---------------------------------
    w('second_order_wamit_forces:')
    w('  MnDrift: '   + d.scalar('MnDrift'))
    w('  NewmanApp: ' + d.scalar('NewmanApp'))
    w('  DiffQTF: '   + d.scalar('DiffQTF'))
    w('  SumQTF: '    + d.scalar('SumQTF'))
    w('')

    #-------------------- additional_stiffness_damping ------------------------------
    # NDOF matches HydroDyn_Input.f90's own computation, needed to know how many
    # values each AddF0/AddCLin/AddBLin/AddBQuad row holds
    if n_body == 1:
        ndof = 6 + int(naddDOF_toks[0])
    else:
        ndof = 6 * vec_multiplier

    w('additional_stiffness_damping:')
    # AddF0: NDOF rows of nWAMITObj values; the section banner (a real, non-comment
    # line) precedes the first row and must be skipped
    addf0_rows = _matrix_rows(d, ndof, n_wamit_obj, skip=1)
    w('  AddF0: [' + ', '.join('[' + ', '.join(row) + ']' for row in addf0_rows) + ']')

    def read_matrix_list(key):
        rows = _matrix_rows(d, ndof, n_wamit_obj * ndof, skip=0)
        mats = []
        for j in range(n_wamit_obj):
            mat = [rows[i][j*ndof:(j+1)*ndof] for i in range(ndof)]
            mats.append('[' + ', '.join('[' + ', '.join(r) + ']' for r in mat) + ']')
        return '  {}: [{}]'.format(key, ', '.join(mats))

    w(read_matrix_list('AddCLin'))
    w(read_matrix_list('AddBLin'))
    w(read_matrix_list('AddBQuad'))
    w('')

    #-------------------- strip_theory -----------------------------------------------
    w('strip_theory:')
    w('  WaveDisp: ' + d.scalar('WaveDisp'))
    w('  AMMod: '    + d.scalar('AMMod'))
    w('  HstMod: '   + d.scalar('HstMod'))
    w('')

    #-------------------- axial_coefficients ------------------------------------------
    # AxCoefID, AxCd, AxCa, AxCp are always present; AxFDMod/AxVnCOff/AxFDLoFSc are
    # optional trailing entries (the text path's 5- and 4-entry fallback reads)
    AX_KEYS = ('AxCoefID', 'AxCd', 'AxCa', 'AxCp', 'AxFDMod', 'AxVnCOff', 'AxFDLoFSc')
    n_ax = int(d.scalar('NAxCoef'))
    # the two header/units lines are always present, even when NAxCoef==0
    ax_rows = d.table_rows(n_ax, skip=2)
    w('axial_coefficients:')
    if n_ax > 0:
        w('  AxialCoefs:')
        for raw in ax_rows:
            w(_row_map(AX_KEYS, _row_tokens(raw)))
    else:
        w('  AxialCoefs: []')
    w('')

    #-------------------- member_joints ------------------------------------------------
    J_KEYS = ('JointID', 'Jointxi', 'Jointyi', 'Jointzi', 'JointAxID', 'JointOvrlp')
    n_joints = int(d.scalar('NJoints'))
    joint_rows = d.table_rows(n_joints, skip=2)
    w('member_joints:')
    if n_joints > 0:
        w('  Joints:')
        for raw in joint_rows:
            toks = _row_tokens(raw)
            if len(toks) != len(J_KEYS):
                raise ValueError('convert_hydrodyn: Joints row "{}" in {} has {} value(s); expected {}'.format(
                    raw, text_path, len(toks), len(J_KEYS)))
            w(_row_map(J_KEYS, toks))
    else:
        w('  Joints: []')
    w('')

    #-------------------- cylindrical_member_cross_section -----------------------------
    CYL_PROP_KEYS = ('PropSetID', 'PropD', 'PropThck')
    n_cyl_props = int(d.scalar('NPropSetsCyl'))
    cyl_prop_rows = d.table_rows(n_cyl_props, skip=2)
    w('cylindrical_member_cross_section:')
    if n_cyl_props > 0:
        w('  MPropSetsCyl:')
        for raw in cyl_prop_rows:
            toks = _row_tokens(raw)
            if len(toks) != len(CYL_PROP_KEYS):
                raise ValueError('convert_hydrodyn: MPropSetsCyl row "{}" in {} has {} value(s); expected {}'.format(
                    raw, text_path, len(toks), len(CYL_PROP_KEYS)))
            w(_row_map(CYL_PROP_KEYS, toks))
    else:
        w('  MPropSetsCyl: []')
    w('')

    #-------------------- rectangular_member_cross_section -----------------------------
    REC_PROP_KEYS = ('PropSetID', 'PropA', 'PropB', 'PropThck')
    n_rec_props = int(d.scalar('NPropSetsRec'))
    rec_prop_rows = d.table_rows(n_rec_props, skip=2)
    w('rectangular_member_cross_section:')
    if n_rec_props > 0:
        w('  MPropSetsRec:')
        for raw in rec_prop_rows:
            toks = _row_tokens(raw)
            if len(toks) != len(REC_PROP_KEYS):
                raise ValueError('convert_hydrodyn: MPropSetsRec row "{}" in {} has {} value(s); expected {}'.format(
                    raw, text_path, len(toks), len(REC_PROP_KEYS)))
            w(_row_map(REC_PROP_KEYS, toks))
    else:
        w('  MPropSetsRec: []')
    w('')

    #-------------------- simple_hydrodynamic_coefficients_cylindrical -----------------
    SIMPL_CYL_KEYS = ('SimplCd', 'SimplCdMG', 'SimplCa', 'SimplCaMG', 'SimplCp', 'SimplCpMG',
                       'SimplAxCd', 'SimplAxCdMG', 'SimplAxCa', 'SimplAxCaMG', 'SimplAxCp',
                       'SimplAxCpMG', 'SimplCb', 'SimplCbMG')
    w('simple_hydrodynamic_coefficients_cylindrical:')
    # unlike the counted tables, there is no keyword line to anchor a forward keyword
    # search on here, so the section banner (a real, non-comment line) must be skipped
    # explicitly too, in addition to the two column-name/units header lines
    toks = _row_tokens(d.table_rows(1, skip=3)[0])
    if len(toks) != len(SIMPL_CYL_KEYS):
        raise ValueError('convert_hydrodyn: simple cylindrical coefficients row in {} has {} value(s); '
                         'expected {}'.format(text_path, len(toks), len(SIMPL_CYL_KEYS)))
    for k, t in zip(SIMPL_CYL_KEYS, toks):
        w('  {}: {}'.format(k, t))
    w('')

    #-------------------- simple_hydrodynamic_coefficients_rectangular -----------------
    SIMPL_REC_KEYS = ('SimplRecCdA', 'SimplRecCdAMG', 'SimplRecCdB', 'SimplRecCdBMG',
                       'SimplRecCaA', 'SimplRecCaAMG', 'SimplRecCaB', 'SimplRecCaBMG',
                       'SimplRecCp', 'SimplRecCpMG', 'SimplRecAxCd', 'SimplRecAxCdMG',
                       'SimplRecAxCa', 'SimplRecAxCaMG', 'SimplRecAxCp', 'SimplRecAxCpMG',
                       'SimplRecCb', 'SimplRecCbMG')
    w('simple_hydrodynamic_coefficients_rectangular:')
    # same reasoning as simple_hydrodynamic_coefficients_cylindrical above: skip the
    # section banner plus the two column-name/units header lines
    toks = _row_tokens(d.table_rows(1, skip=3)[0])
    if len(toks) != len(SIMPL_REC_KEYS):
        raise ValueError('convert_hydrodyn: simple rectangular coefficients row in {} has {} value(s); '
                         'expected {}'.format(text_path, len(toks), len(SIMPL_REC_KEYS)))
    for k, t in zip(SIMPL_REC_KEYS, toks):
        w('  {}: {}'.format(k, t))
    w('')

    #-------------------- depth_based_hydrodynamic_coefficients_cylindrical -----------
    DPTH_CYL_KEYS = ('Dpth', 'DpthCd', 'DpthCdMG', 'DpthCa', 'DpthCaMG', 'DpthCp', 'DpthCpMG',
                      'DpthAxCd', 'DpthAxCdMG', 'DpthAxCa', 'DpthAxCaMG', 'DpthAxCp',
                      'DpthAxCpMG', 'DpthCb', 'DpthCbMG')
    n_dpth_cyl = int(d.scalar('NCoefDpthCyl'))
    dpth_cyl_rows = d.table_rows(n_dpth_cyl, skip=2)
    w('depth_based_hydrodynamic_coefficients_cylindrical:')
    if n_dpth_cyl > 0:
        w('  CoefDpthsCyl:')
        for raw in dpth_cyl_rows:
            toks = _row_tokens(raw)
            if len(toks) != len(DPTH_CYL_KEYS):
                raise ValueError('convert_hydrodyn: CoefDpthsCyl row "{}" in {} has {} value(s); expected {}'.format(
                    raw, text_path, len(toks), len(DPTH_CYL_KEYS)))
            w(_row_map(DPTH_CYL_KEYS, toks))
    else:
        w('  CoefDpthsCyl: []')
    w('')

    #-------------------- depth_based_hydrodynamic_coefficients_rectangular -----------
    DPTH_REC_KEYS = ('Dpth', 'DpthCdA', 'DpthCdAMG', 'DpthCdB', 'DpthCdBMG', 'DpthCaA',
                      'DpthCaAMG', 'DpthCaB', 'DpthCaBMG', 'DpthCp', 'DpthCpMG', 'DpthAxCd',
                      'DpthAxCdMG', 'DpthAxCa', 'DpthAxCaMG', 'DpthAxCp', 'DpthAxCpMG',
                      'DpthCb', 'DpthCbMG')
    n_dpth_rec = int(d.scalar('NCoefDpthRec'))
    dpth_rec_rows = d.table_rows(n_dpth_rec, skip=2)
    w('depth_based_hydrodynamic_coefficients_rectangular:')
    if n_dpth_rec > 0:
        w('  CoefDpthsRec:')
        for raw in dpth_rec_rows:
            toks = _row_tokens(raw)
            if len(toks) != len(DPTH_REC_KEYS):
                raise ValueError('convert_hydrodyn: CoefDpthsRec row "{}" in {} has {} value(s); expected {}'.format(
                    raw, text_path, len(toks), len(DPTH_REC_KEYS)))
            w(_row_map(DPTH_REC_KEYS, toks))
    else:
        w('  CoefDpthsRec: []')
    w('')

    #-------------------- member_based_hydrodynamic_coefficients_cylindrical ---------
    MEMB_CYL_KEYS = ('MemberID', 'MemberCd1', 'MemberCd2', 'MemberCdMG1', 'MemberCdMG2',
                      'MemberCa1', 'MemberCa2', 'MemberCaMG1', 'MemberCaMG2',
                      'MemberCp1', 'MemberCp2', 'MemberCpMG1', 'MemberCpMG2',
                      'MemberAxCd1', 'MemberAxCd2', 'MemberAxCdMG1', 'MemberAxCdMG2',
                      'MemberAxCa1', 'MemberAxCa2', 'MemberAxCaMG1', 'MemberAxCaMG2',
                      'MemberAxCp1', 'MemberAxCp2', 'MemberAxCpMG1', 'MemberAxCpMG2',
                      'MemberCb1', 'MemberCb2', 'MemberCbMG1', 'MemberCbMG2')
    n_memb_cyl = int(d.scalar('NCoefMembersCyl'))
    memb_cyl_rows = d.table_rows(n_memb_cyl, skip=2)
    w('member_based_hydrodynamic_coefficients_cylindrical:')
    if n_memb_cyl > 0:
        w('  CoefMembersCyl:')
        for raw in memb_cyl_rows:
            toks = _row_tokens(raw)
            if len(toks) != len(MEMB_CYL_KEYS):
                raise ValueError('convert_hydrodyn: CoefMembersCyl row "{}" in {} has {} value(s); '
                                 'expected {}'.format(raw, text_path, len(toks), len(MEMB_CYL_KEYS)))
            w(_row_map(MEMB_CYL_KEYS, toks))
    else:
        w('  CoefMembersCyl: []')
    w('')

    #-------------------- member_based_hydrodynamic_coefficients_rectangular ---------
    MEMB_REC_KEYS = ('MemberID', 'MemberCdA1', 'MemberCdA2', 'MemberCdAMG1', 'MemberCdAMG2',
                      'MemberCdB1', 'MemberCdB2', 'MemberCdBMG1', 'MemberCdBMG2',
                      'MemberCaA1', 'MemberCaA2', 'MemberCaAMG1', 'MemberCaAMG2',
                      'MemberCaB1', 'MemberCaB2', 'MemberCaBMG1', 'MemberCaBMG2',
                      'MemberCp1', 'MemberCp2', 'MemberCpMG1', 'MemberCpMG2',
                      'MemberAxCd1', 'MemberAxCd2', 'MemberAxCdMG1', 'MemberAxCdMG2',
                      'MemberAxCa1', 'MemberAxCa2', 'MemberAxCaMG1', 'MemberAxCaMG2',
                      'MemberAxCp1', 'MemberAxCp2', 'MemberAxCpMG1', 'MemberAxCpMG2',
                      'MemberCb1', 'MemberCb2', 'MemberCbMG1', 'MemberCbMG2')
    n_memb_rec = int(d.scalar('NCoefMembersRec'))
    memb_rec_rows = d.table_rows(n_memb_rec, skip=2)
    w('member_based_hydrodynamic_coefficients_rectangular:')
    if n_memb_rec > 0:
        w('  CoefMembersRec:')
        for raw in memb_rec_rows:
            toks = _row_tokens(raw)
            if len(toks) != len(MEMB_REC_KEYS):
                raise ValueError('convert_hydrodyn: CoefMembersRec row "{}" in {} has {} value(s); '
                                 'expected {}'.format(raw, text_path, len(toks), len(MEMB_REC_KEYS)))
            w(_row_map(MEMB_REC_KEYS, toks))
    else:
        w('  CoefMembersRec: []')
    w('')

    #-------------------- members -----------------------------------------------------
    # FDMod/VnCOffA/VnCOffB/FDLoFScA/FDLoFScB are optional trailing entries (the text
    # path's 11-entry fallback read); PropPot is the one LOGICAL entry in this table
    MEMBER_KEYS = ('MemberID', 'MJointID1', 'MJointID2', 'MPropSetID1', 'MPropSetID2',
                    'MSecGeom', 'MSpinOrient', 'MDivSize', 'MCoefMod', 'MHstLMod', 'PropPot',
                    'FDMod', 'VnCOffA', 'VnCOffB', 'FDLoFScA', 'FDLoFScB')
    n_members = int(d.scalar('NMembers'))
    member_rows = d.table_rows(n_members, skip=2)
    w('members:')
    if n_members > 0:
        w('  Members:')
        for raw in member_rows:
            toks = _row_tokens(raw)
            if len(toks) not in (11, 16):
                raise ValueError('convert_hydrodyn: Members row "{}" in {} has {} value(s); '
                                 'expected 11 or 16'.format(raw, text_path, len(toks)))
            vals = list(toks)
            vals[10] = _as_bool(vals[10])  # PropPot
            w(_row_map(MEMBER_KEYS, vals))
    else:
        w('  Members: []')
    w('')

    #-------------------- filled_members -----------------------------------------------
    # FillNumM (the row's first token) is consumed only to know how many member IDs
    # follow; it is never itself emitted (FillNumM is derived from the FillMList length)
    n_fill = int(d.scalar('NFillGroups'))
    fill_rows = d.table_rows(n_fill, skip=2)
    w('filled_members:')
    if n_fill > 0:
        w('  FilledGroups:')
        for raw in fill_rows:
            toks = _row_tokens(raw)
            fill_num_m = int(toks[0])
            fill_mlist = toks[1:1 + fill_num_m]
            fill_fsloc = toks[1 + fill_num_m]
            fill_dens = _default_or_num(toks[2 + fill_num_m])
            # block-style item (see _row_map's docstring for why: a flow mapping here
            # would be misparsed as "key: value" split on its first colon)
            w('    - FillMList: [{}]'.format(', '.join(fill_mlist)))
            w('      FillFSLoc: {}'.format(fill_fsloc))
            w('      FillDens: {}'.format(fill_dens))
    else:
        w('  FilledGroups: []')
    w('')

    #-------------------- marine_growth_by_depth ---------------------------------------
    MG_KEYS = ('MGDpth', 'MGThck', 'MGDens')
    n_mg = int(d.scalar('NMGDepths'))
    mg_rows = d.table_rows(n_mg, skip=2)
    w('marine_growth_by_depth:')
    if n_mg > 0:
        w('  MGDepths:')
        for raw in mg_rows:
            toks = _row_tokens(raw)
            if len(toks) != len(MG_KEYS):
                raise ValueError('convert_hydrodyn: MGDepths row "{}" in {} has {} value(s); expected {}'.format(
                    raw, text_path, len(toks), len(MG_KEYS)))
            w(_row_map(MG_KEYS, toks))
    else:
        w('  MGDepths: []')
    w('')

    #-------------------- member_output_list -------------------------------------------
    # NOutLoc (the row's second token) is consumed only to know how many node
    # locations follow; it is never itself emitted (derived from the NodeLocs length)
    n_mout = int(d.scalar('NMOutputs'))
    mout_rows = d.table_rows(n_mout, skip=2)
    w('member_output_list:')
    if n_mout > 0:
        w('  MOutLst:')
        for raw in mout_rows:
            toks = _row_tokens(raw)
            member_id = toks[0]
            n_out_loc = int(toks[1])
            node_locs = toks[2:2 + n_out_loc]
            # block-style item (see _row_map's docstring for why)
            w('    - MemberID: {}'.format(member_id))
            w('      NodeLocs: [{}]'.format(', '.join(node_locs)))
    else:
        w('  MOutLst: []')
    w('')

    #-------------------- joint_output_list ---------------------------------------------
    # a flat list of joint IDs (not itself tabular; no table header lines precede it)
    n_jout = int(d.scalar('NJOutputs'))
    jout_toks = [t.rstrip(',') for t in d.find('JOutLst')]
    w('joint_output_list:')
    if n_jout > 0:
        w('  JOutLst: [' + ', '.join(jout_toks[:n_jout]) + ']')
    else:
        w('  JOutLst: []')
    w('')

    #-------------------- output -----------------------------------------------------
    w('output:')
    w('  HDSum: '    + _as_bool(d.scalar('HDSum')))
    w('  OutAll: '   + _as_bool(d.scalar('OutAll')))
    w('  OutSwtch: ' + d.scalar('OutSwtch'))
    w('  OutFmt: '   + _as_str(d.scalar('OutFmt')))
    w('  OutSFmt: '  + _as_str(d.scalar('OutSFmt')))
    w('')

    #-------------------- output_channels ----------------------------------------------
    w('output_channels:')
    # like SeaState's text format, there is no literal "OutList" keyword line -- channel
    # names start immediately after the "OUTPUT CHANNELS" section banner
    channels = d.outlist(keyword='OUTPUT CHANNELS')
    w('  OutList: [' + ', '.join('"' + c + '"' for c in channels) + ']')
    w('')

    return '\n'.join(out)


def convert_hydrodyn_driver(text_path):
    """Convert a text-format HydroDyn driver input file (Wave 4, class-B/sequential-
    reader driver -- follows the seastate/subdyn convention) to its YAML schema
    (modules/hydrodyn/src/HydroDyn_Driver_Yaml.f90 is the source of truth).

    Schema mirrors the driver's text reader (HydroDyn_DriverSubs.f90:110-317,
    ReadDriverInputFile) key-for-key:
        general:                   Echo, FTitle (line 2's free-text description, read
                                    via ReadStr with no keyword -- like convert_fst's
                                    "description", grabbed by line index)
        environmental_conditions:  Gravity, WtrDens, WtrDpth, MSL2SWL
        hydrodyn:                  HDInputFile, SeaStateInputFile, OutRootName,
                                    Linearize, NSteps, TimeInterval
        prp_inputs:                PRPInputsMod, NAddDOF, PtfmRefzt, PRPInputsFile
        prp_steady_state_inputs:   uPRPInSteady, uDotPRPInSteady, uDotDotPRPInSteady
                                    -- always present (the text path's three ReadAry
                                    calls are unconditional; PRPInputsMod only gates
                                    whether the *parser* zeroes them afterward, not
                                    whether they are read)

    HDInputFile/SeaStateInputFile/OutRootName/PRPInputsFile stay path-valued
    (second-order rule). HDInputFile is NOT repointed at a .yaml sibling here -- that
    repointing, when needed, is done by the caller after this converter returns,
    mirroring subdyn's driver-mode harness convention (SeaStateInputFile conversion is
    out of scope, same as the primary-only executeYamlEquivalenceCase.py hydrodyn
    block)."""
    d = _TextDeck(text_path)

    # line 1 is a pure banner comment (ReadCom); line 2 is the free-text description,
    # read verbatim via ReadStr (no keyword, not a keyword-value line)
    ftitle = d.lines[1] if len(d.lines) > 1 else ''
    d.cursor = 2

    out = []
    w = out.append
    w('# HydroDyn driver input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('  FTitle: ' + _quote_line(ftitle))
    w('')

    w('environmental_conditions:')
    w('  Gravity: ' + d.scalar('Gravity'))
    w('  WtrDens: ' + d.scalar('WtrDens'))
    w('  WtrDpth: ' + d.scalar('WtrDpth'))
    w('  MSL2SWL: ' + d.scalar('MSL2SWL'))
    w('')

    w('hydrodyn:')
    w('  HDInputFile: ' + _as_str(d.scalar('HDInputFile')))
    w('  SeaStateInputFile: ' + _as_str(d.scalar('SeaStateInputFile')))
    w('  OutRootName: ' + _as_str(d.scalar('OutRootName')))
    w('  Linearize: ' + _as_bool(d.scalar('Linearize')))
    w('  NSteps: ' + d.scalar('NSteps'))
    w('  TimeInterval: ' + d.scalar('TimeInterval'))
    w('')

    w('prp_inputs:')
    w('  PRPInputsMod: ' + d.scalar('PRPInputsMod'))
    w('  NAddDOF: ' + d.scalar('NAddDOF'))
    w('  PtfmRefzt: ' + d.scalar('PtfmRefzt'))
    w('  PRPInputsFile: ' + _as_str(d.scalar('PRPInputsFile')))
    w('')

    w('prp_steady_state_inputs:')
    w('  uPRPInSteady: [' + _list_join(d.find('uPRPInSteady')) + ']')
    w('  uDotPRPInSteady: [' + _list_join(d.find('uDotPRPInSteady')) + ']')
    w('  uDotDotPRPInSteady: [' + _list_join(d.find('uDotDotPRPInSteady')) + ']')
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


# AeroDyn's Rotor/Blade Properties section never repeats a "MaxBl" count keyword; the
# text format always reads exactly max(MaxBl, sum(NumBlades)) ADBlFile lines (MaxBl == 3,
# AeroDyn_IO_Params.f90), regardless of the true blade count. Every r-test AeroDyn primary
# file uses conventional 3-bladed rotors, so this matches sum(NumBlades) exactly for all
# supported cases; the true blade count is otherwise not recoverable from the primary
# file alone (it is supplied externally by the driver/glue code).
_AD_MAXBL = 3

_AD_TWR_KEYS = ('TwrElev', 'TwrDiam', 'TwrCd', 'TwrTI', 'TwrCb', 'TwrCp', 'TwrCa')


def convert_aerodyn(text_path, n_rotors=1, num_blades_total=None):
    """Convert a text-format AeroDyn primary input file to its YAML schema
    (modules/aerodyn/src/AeroDyn_Yaml.f90 is the source of truth).

    Sections mirror the text file's banners (general, environmental_conditions,
    bemt_options, dbemt_options, olaf_options, unsteady_aero_options, airfoil_info,
    rotor_blade_properties, hub_properties, nacelle_properties,
    tail_fin_aerodynamics, tower_influence, outputs, output_channels, nodal_outputs).
    Legacy-only keys (WakeMod, AFAeroMod, SkewMod, FrozenWake, UAMod, and the
    "SkewModFactor" alias) have no YAML equivalent and are skipped; every r-test
    AeroDyn primary file already uses the current key names.

    Airfoil/blade/tailfin/AeroAcoustics/OLAF files stay path strings (never inlined).
    NumAFfiles/NumTwrNds/NBlOuts/NTwOuts/NumOuts/BldNd_NumOuts are all consumed here
    only to know how many entries/rows to read, and are never themselves emitted
    (AeroDyn_Yaml.f90 derives them back from list length).

    `n_rotors`: the rotor count, needed because hub_properties/nacelle_properties/
    tail_fin_aerodynamics/tower_influence each repeat once per rotor in the text format,
    and NumBlades/rotor-count is supplied externally by the driver/glue code, never read
    from the AeroDyn primary file itself. The .fst converter passes its own NRotors;
    callers converting a standalone AeroDyn-driver case that know the driver's turbine
    count should pass it explicitly too (default 1 covers the common single-rotor case).

    `num_blades_total`: the true blade count summed across all rotors (AeroDyn's own
    NumBlades is likewise supplied externally, from ElastoDyn's NumBl -- it is never read
    from the AeroDyn primary file). The text format always reads exactly
    max(MaxBl=3, num_blades_total) ADBlFile lines regardless of the true blade count, so
    the converter must know the true count to correctly drop unused padding lines (e.g. a
    2-bladed turbine still has 3 ADBlFile lines in the text file, but only the first 2 are
    ever used -- the .fst converter passes ElastoDyn's own NumBl; the default (None -> 3 *
    n_rotors) covers the common 3-bladed-rotor case, e.g. the standalone-driver
    equivalence tests' BAR-turbine cases)."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# AeroDyn primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    #-------------------- general -----------------------------------------------------
    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('  DTAero: ' + _default_or_num(d.scalar('DTAero')))
    w('  Wake_Mod: ' + d.scalar('Wake_Mod'))
    w('  TwrPotent: ' + d.scalar('TwrPotent'))
    w('  TwrShadow: ' + d.scalar('TwrShadow'))
    w('  TwrAero: ' + _as_bool(d.scalar('TwrAero')))
    w('  CavitCheck: ' + _as_bool(d.scalar('CavitCheck')))
    w('  NacelleDrag: ' + _as_bool(d.scalar('NacelleDrag')))
    w('  CompAA: ' + _as_bool(d.scalar('CompAA')))
    w('  AA_InputFile: ' + _as_str(d.scalar('AA_InputFile')))
    w('')

    #-------------------- environmental_conditions -------------------------------------
    w('environmental_conditions:')
    w('  AirDens: ' + _default_or_num(d.scalar('AirDens')))
    w('  KinVisc: ' + _default_or_num(d.scalar('KinVisc')))
    w('  SpdSound: ' + _default_or_num(d.scalar('SpdSound')))
    w('  Patm: ' + _default_or_num(d.scalar('Patm')))
    w('  Pvap: ' + _default_or_num(d.scalar('Pvap')))
    w('')

    #-------------------- bemt_options --------------------------------------------------
    # consume the BEMT section banner explicitly: its own bracketed usage note ("[unused
    # when Wake_Mod=0 or 3, except for BEM_Mod]") mentions "BEM_Mod" ahead of the actual
    # data line, which would otherwise collide with the literal-substring keyword search
    d.find('Momentum Theory Options')
    w('bemt_options:')
    w('  BEM_Mod: ' + d.scalar('BEM_Mod'))
    w('  Skew_Mod: ' + d.scalar('Skew_Mod'))
    w('  SkewMomCorr: ' + _as_bool(d.scalar('SkewMomCorr')))
    w('  SkewRedistr_Mod: ' + _default_or_num(d.scalar('SkewRedistr_Mod')))
    w('  SkewRedistrFactor: ' + _default_or_num(d.scalar('SkewRedistrFactor')))
    w('  TipLoss: ' + _as_bool(d.scalar('TipLoss')))
    w('  HubLoss: ' + _as_bool(d.scalar('HubLoss')))
    w('  TanInd: ' + _as_bool(d.scalar('TanInd')))
    w('  AIDrag: ' + _as_bool(d.scalar('AIDrag')))
    w('  TIDrag: ' + _as_bool(d.scalar('TIDrag')))
    w('  IndToler: ' + _default_or_num(d.scalar('IndToler')))
    w('  MaxIter: ' + d.scalar('MaxIter'))
    w('  SectAvg: ' + _as_bool(d.scalar('SectAvg')))
    w('  SectAvgWeighting: ' + _default_or_num(d.scalar('SectAvgWeighting')))
    w('  SectAvgNPoints: ' + _default_or_num(d.scalar('SectAvgNPoints')))
    w('  SectAvgPsiBwd: ' + _default_or_num(d.scalar('SectAvgPsiBwd')))
    w('  SectAvgPsiFwd: ' + _default_or_num(d.scalar('SectAvgPsiFwd')))
    w('')

    #-------------------- dbemt_options --------------------------------------------------
    w('dbemt_options:')
    w('  DBEMT_Mod: ' + d.scalar('DBEMT_Mod'))
    w('  tau1_const: ' + d.scalar('tau1_const'))
    w('')

    #-------------------- olaf_options -----------------------------------------------
    w('olaf_options:')
    w('  OLAFInputFileName: ' + _as_str(d.scalar('OLAFInputFileName')))
    w('')

    #-------------------- unsteady_aero_options ----------------------------------------
    w('unsteady_aero_options:')
    w('  AoA34: ' + _as_bool(d.scalar('AoA34')))
    w('  UA_Mod: ' + d.scalar('UA_Mod'))
    w('  FLookup: ' + _as_bool(d.scalar('FLookup')))
    w('  IntegrationMethod: ' + d.scalar('IntegrationMethod'))
    w('  UAStartRad: ' + d.scalar('UAStartRad'))
    w('  UAEndRad: ' + d.scalar('UAEndRad'))
    w('')

    #-------------------- airfoil_info --------------------------------------------------
    w('airfoil_info:')
    w('  AFTabMod: ' + d.scalar('AFTabMod'))
    w('  InCol_Alfa: ' + d.scalar('InCol_Alfa'))
    w('  InCol_Cl: ' + d.scalar('InCol_Cl'))
    w('  InCol_Cd: ' + d.scalar('InCol_Cd'))
    w('  InCol_Cm: ' + d.scalar('InCol_Cm'))
    w('  InCol_Cpmin: ' + d.scalar('InCol_Cpmin'))
    n_af = int(d.scalar('NumAFfiles'))
    af_first = d.find('AFNames')[0]
    af_rest = _bare_lines(d, n_af - 1) if n_af > 1 else []
    af_toks = [af_first] + af_rest
    w('  AFNames: [' + ', '.join(_as_str(t) for t in af_toks) + ']')
    w('')

    #-------------------- rotor_blade_properties ---------------------------------------
    w('rotor_blade_properties:')
    w('  UseBlCm: ' + _as_bool(d.scalar('UseBlCm')))
    if num_blades_total is None:
        num_blades_total = _AD_MAXBL * n_rotors
    n_blade_lines = max(_AD_MAXBL, num_blades_total)
    adbl_first = d.find('ADBlFile')[0]
    adbl_rest = _bare_lines(d, n_blade_lines - 1) if n_blade_lines > 1 else []
    adbl_toks = ([adbl_first] + adbl_rest)[:num_blades_total]
    w('  ADBlFile: [' + ', '.join(_as_str(t) for t in adbl_toks) + ']')
    w('')

    #-------------------- hub_properties / nacelle_properties / tail_fin_aerodynamics /
    #-------------------- tower_influence (one entry per rotor) ------------------------
    w('hub_properties:')
    for _ in range(n_rotors):
        w('  - VolHub: ' + d.scalar('VolHub'))
        w('    HubCenBx: ' + d.scalar('HubCenBx'))
    w('')

    w('nacelle_properties:')
    for _ in range(n_rotors):
        w('  - VolNac: ' + d.scalar('VolNac'))
        w('    NacCenB: [' + _list_join(d.find('NacCenB')[:3]) + ']')
        w('    NacArea: [' + _list_join(d.find('NacArea')[:3]) + ']')
        w('    NacCd: [' + _list_join(d.find('NacCd')[:3]) + ']')
        w('    NacDragAC: [' + _list_join(d.find('NacDragAC')[:3]) + ']')
    w('')

    w('tail_fin_aerodynamics:')
    for _ in range(n_rotors):
        w('  - TFinAero: ' + _as_bool(d.scalar('TFinAero')))
        w('    TFinFile: ' + _as_str(d.scalar('TFinFile')))
    w('')

    w('tower_influence:')
    for _ in range(n_rotors):
        n_twr = int(d.scalar('NumTwrNds'))
        # the two header/units lines are always present, even when NumTwrNds==0
        twr_rows = _matrix_rows(d, n_twr, 7, skip=2)
        if n_twr > 0:
            w('  - tower_nodes:')
            for row in twr_rows:
                w(_row_map(_AD_TWR_KEYS, row, indent='    '))
        else:
            w('  - tower_nodes: []')
    w('')

    #-------------------- outputs / output_channels ------------------------------------
    w('outputs:')
    w('  SumPrint: ' + _as_bool(d.scalar('SumPrint')))
    n_bl_outs = int(d.scalar('NBlOuts'))
    w('  BlOutNd: [' + _list_join(d.find('BlOutNd')[:n_bl_outs]) + ']')
    n_tw_outs = int(d.scalar('NTwOuts'))
    w('  TwOutNd: [' + _list_join(d.find('TwOutNd')[:n_tw_outs]) + ']')
    w('')

    w('output_channels:')
    channels = d.outlist('OutList')
    w('  OutList: [' + ', '.join('"' + c + '"' for c in channels) + ']')
    w('')

    #-------------------- nodal_outputs -------------------------------------------------
    # The text path tolerates a completely missing Nodal Outputs section (legacy files:
    # ParsePrimaryFileInfo's FailedNodal degrades to BldNd_BladesOut=0/BldNd_NumOuts=0
    # with a warning). The YAML schema requires the section, so when the text deck omits
    # it, emit the explicit equivalent of that degrade: 0 blades out + an empty channel
    # list (BldNd_BlOutNd is never parsed downstream when BldNd_BladesOut<=0, so "ALL"
    # is an inert placeholder).
    w('nodal_outputs:')
    has_nodal = any(_tokens_before_keyword(line, 'BldNd_BladesOut') is not None
                    for line in d.lines[d.cursor:])
    if has_nodal:
        w('  BldNd_BladesOut: ' + d.scalar('BldNd_BladesOut'))
        bl_outnd_toks = d.find('BldNd_BlOutNd')
        bl_outnd_val = ', '.join(t.rstrip(',') for t in bl_outnd_toks)
        w('  BldNd_BlOutNd: ' + _as_str(bl_outnd_val))
        nodal_channels = d.outlist('OutList_Nodal')
        w('  BldNd_OutList: [' + ', '.join('"' + c + '"' for c in nodal_channels) + ']')
    else:
        w('  BldNd_BladesOut: 0')
        w('  BldNd_BlOutNd: "ALL"')
        w('  BldNd_OutList: []')
    w('')

    return '\n'.join(out)


def _ad_driver_turbine_lines(d, iwt):
    """Build the YAML lines for one `turbines` list entry (turbine index `iwt`, 1-based),
    mirroring the text reader's per-turbine loop body (Dvr_ReadInputFile:1064-1271,
    AeroDyn_Driver_Subs.f90) key-for-key. Returns a list of already-indented lines (the
    first starting with the list-item dash); the caller appends them under `turbines:`.

    Basic format (BasicHAWTFormat(iwt)==True): baseOriginInit + 7 scalars; blade
    orientation is derived from `precone`, never read, so no per-blade list at all.

    Advanced format: geometry (baseOrientationInit, hasTower, HAWTprojection,
    twrOrigin_t, nacOrigin_t, hubOrigin_n, hubOrientation_n, numBlades) followed by three
    separate per-blade passes in the text file (BldOrigin_h, then BldOrientation_h, then
    BldHubRad_bl, each looped over all blades) -- collected here and merged with the
    later motion-section's per-blade values (BldPitch or BldMotionFileName, depending on
    BldMotionType) into ONE combined `blades:` list, one block-mapping row per blade,
    rather than mirroring the text file's split geometry-loop/motion-loop structure.

    Base motion (baseMotionType/degreeOfFreedom/amplitude/frequency/baseMotionFileName)
    and, for advanced format, the whole RNA-motion section (down to BldMotionFileName)
    are physically present in the text file -- and so always emitted here -- regardless
    of AnalysisType; AeroDyn_Driver_Yaml.f90 reads/uses them exactly as conditionally as
    the text path does (only their *use* is analysisType-gated, not their presence)."""
    sWT = '({})'.format(iwt)
    lines = []
    tw = lines.append

    basic_tok = d.scalar('BasicHAWTFormat' + sWT)
    basic = _unquote(basic_tok).strip().lower() in ('true', 't', '.true.')
    tw('  - basicHAWTFormat: ' + _as_bool(basic_tok))

    base_origin = _flatten_csv(d.find('BaseOriginInit' + sWT))[:3]
    tw('    baseOriginInit: [' + _list_join(base_origin) + ']')

    num_blades = 0
    bld_origin_h, bld_orient_h, bld_hubrad = [], [], []

    if basic:
        num_blades_tok = d.scalar('NumBlades' + sWT)
        num_blades = int(_unquote(num_blades_tok))
        tw('    numBlades: ' + num_blades_tok)
        tw('    hubRad: ' + d.scalar('HubRad' + sWT))
        tw('    hubHt: ' + d.scalar('HubHt' + sWT))
        tw('    overhang: ' + d.scalar('Overhang' + sWT))
        tw('    shftTilt: ' + d.scalar('ShftTilt' + sWT))
        tw('    precone: ' + d.scalar('Precone' + sWT))
        tw('    twr2Shft: ' + d.scalar('Twr2Shft' + sWT))
    else:
        base_orient = _flatten_csv(d.find('BaseOrientationInit' + sWT))[:3]
        tw('    baseOrientationInit: [' + _list_join(base_orient) + ']')
        tw('    hasTower: ' + _as_bool(d.scalar('HasTower' + sWT)))
        tw('    HAWTprojection: ' + _as_bool(d.scalar('HAWTprojection' + sWT)))
        twr_origin = _flatten_csv(d.find('TwrOrigin_t' + sWT))[:3]
        tw('    twrOrigin_t: [' + _list_join(twr_origin) + ']')
        nac_origin = _flatten_csv(d.find('NacOrigin_t' + sWT))[:3]
        tw('    nacOrigin_t: [' + _list_join(nac_origin) + ']')
        hub_origin = _flatten_csv(d.find('HubOrigin_n' + sWT))[:3]
        tw('    hubOrigin_n: [' + _list_join(hub_origin) + ']')
        hub_orient = _flatten_csv(d.find('HubOrientation_n' + sWT))[:3]
        tw('    hubOrientation_n: [' + _list_join(hub_orient) + ']')

        num_blades_tok = d.scalar('NumBlades' + sWT)
        num_blades = int(_unquote(num_blades_tok))
        tw('    numBlades: ' + num_blades_tok)

        for ib in range(1, num_blades + 1):
            sBld = '({}_{})'.format(iwt, ib)
            bld_origin_h.append(_flatten_csv(d.find('BldOrigin_h' + sBld))[:3])
        for ib in range(1, num_blades + 1):
            sBld = '({}_{})'.format(iwt, ib)
            bld_orient_h.append(_flatten_csv(d.find('BldOrientation_h' + sBld))[:3])
        for ib in range(1, num_blades + 1):
            sBld = '({}_{})'.format(iwt, ib)
            bld_hubrad.append(d.scalar('BldHubRad_bl' + sBld))

    #------------------------------ base motion (common) -------------------------------
    tw('    baseMotionType: ' + d.scalar('BaseMotionType' + sWT))
    tw('    degreeOfFreedom: ' + d.scalar('DegreeOfFreedom' + sWT))
    tw('    amplitude: ' + d.scalar('Amplitude' + sWT))
    tw('    frequency: ' + d.scalar('Frequency' + sWT))
    tw('    baseMotionFileName: ' + _as_str(d.scalar('BaseMotionFileName' + sWT)))

    #------------------------------ RNA motion ------------------------------------------
    if basic:
        tw('    nacYaw: ' + d.scalar('NacYaw' + sWT))
        tw('    rotSpeed: ' + d.scalar('RotSpeed' + sWT))
        tw('    bldPitch: ' + d.scalar('BldPitch' + sWT))
    elif num_blades > 0:
        tw('    nacMotionType: ' + d.scalar('NacMotionType' + sWT))
        tw('    nacYaw: ' + d.scalar('NacYaw' + sWT))
        tw('    nacMotionFileName: ' + _as_str(d.scalar('NacMotionFileName' + sWT)))
        tw('    rotMotionType: ' + d.scalar('RotMotionType' + sWT))
        tw('    rotSpeed: ' + d.scalar('RotSpeed' + sWT))
        tw('    rotMotionFileName: ' + _as_str(d.scalar('RotMotionFileName' + sWT)))
        bld_motion_type_tok = d.scalar('BldMotionType' + sWT)
        bld_motion_type = int(_unquote(bld_motion_type_tok))
        tw('    bldMotionType: ' + bld_motion_type_tok)

        bld_pitch = [None] * num_blades
        bld_motion_file = [None] * num_blades
        if bld_motion_type == 0:
            for ib in range(1, num_blades + 1):
                sBld = '({}_{})'.format(iwt, ib)
                bld_pitch[ib-1] = d.scalar('BldPitch' + sBld)
        else:
            for ib in range(1, num_blades + 1):
                sBld = '({}_{})'.format(iwt, ib)
                bld_motion_file[ib-1] = d.scalar('BldMotionFileName' + sBld)

        # merged geometry + motion, one combined block-mapping row per blade
        tw('    blades:')
        for ib in range(num_blades):
            tw('      - origin_h: [' + _list_join(bld_origin_h[ib]) + ']')
            tw('        orientation_h: [' + _list_join(bld_orient_h[ib]) + ']')
            tw('        hubRad_bl: ' + bld_hubrad[ib])
            if bld_motion_type == 0:
                tw('        bldPitch: ' + bld_pitch[ib])
            else:
                tw('        bldMotionFileName: ' + _as_str(bld_motion_file[ib]))

    return lines


def convert_aerodyn_driver(text_path):
    """Convert a text-format AeroDyn driver input file (Wave 4, class-A/FileInfoType-
    ParseVar reader, the largest driver schema) to its YAML schema
    (modules/aerodyn/src/AeroDyn_Driver_Yaml.f90 is the source of truth).

    Schema mirrors the driver's text reader (Dvr_ReadInputFile,
    AeroDyn_Driver_Subs.f90:963-1361) key-for-key:
        general:                  Echo
        configuration:             MHK, analysisType, tMax, dt, AeroFile
        environmental_conditions: FldDens, KinVisc, SpdSound, Patm, Pvap, WtrDpth
        inflow:                   compInflow, InflowFile, HWindSpeed, RefHt, PLExp --
                                   the last three are always physically present in the
                                   text file (and so always emitted here) regardless of
                                   compInflow
        seastate:                 CompSeaSt, SeaStFile
        turbines:                 a list of block mappings, one per turbine (length =
                                   NumTurbines -- counts derive from list length, never
                                   a separate key); see _ad_driver_turbine_lines for the
                                   per-turbine schema (basic vs. advanced geometry, the
                                   merged `blades:` list, base/RNA motion)
        time_dependent_analysis:  TimeAnalysisFileName -- always physically present in
                                   the text file (a data or comment-skip line depending
                                   on AnalysisType), so always emitted here too;
                                   AeroDyn_Driver_Yaml.f90 only reads it when
                                   analysisType == 2
        combined_case_analysis:   cases -- a list of block-mapping rows (10 columns:
                                   HWindSpeed, PLExp, rotSpeed, bldPitch, nacYaw, dT,
                                   tMax, DOF, amplitude, frequency) via the _row_map
                                   helper (SubDyn 4.3a lesson: YamlInput's block-sequence
                                   reader rejects flow mappings). NumCases + its two
                                   header/units lines are always physically present
                                   (possibly followed by zero data rows), so always
                                   parsed and emitted here (as an empty list when
                                   NumCases==0); AeroDyn_Driver_Yaml.f90 only reads it
                                   when analysisType == 3
        outputs:                  outFmt, outFileFmt, WrVTK, WrVTK_Type, VTKHubRad,
                                   VTKNacDim[6]

    AeroFile is repointed at its converted *.yaml sibling (second-order rule for the
    AeroDyn primary file: the driver-mode harness runs a fully-YAML case, so the
    driver's own reference must follow), same convention as convert_aerodisk_driver.
    InflowFile/SeaStFile/baseMotionFileName/nacMotionFileName/rotMotionFileName/
    bldMotionFileName/TimeAnalysisFileName all stay path-valued (second-order rule)."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# AeroDyn driver input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('general:')
    w('  Echo: ' + _as_bool(d.scalar('Echo')))
    w('')

    w('configuration:')
    w('  MHK: ' + d.scalar('MHK'))
    w('  analysisType: ' + d.scalar('AnalysisType'))
    w('  tMax: ' + d.scalar('TMax'))
    w('  dt: ' + d.scalar('DT'))
    aero_file = _unquote(d.scalar('AeroFile'))
    aero_file = os.path.splitext(aero_file)[0] + '.yaml'
    w('  AeroFile: ' + _as_str(aero_file))
    w('')

    w('environmental_conditions:')
    w('  FldDens: ' + d.scalar('FldDens'))
    w('  KinVisc: ' + d.scalar('KinVisc'))
    w('  SpdSound: ' + d.scalar('SpdSound'))
    w('  Patm: ' + d.scalar('Patm'))
    w('  Pvap: ' + d.scalar('Pvap'))
    w('  WtrDpth: ' + d.scalar('WtrDpth'))
    w('')

    w('inflow:')
    w('  compInflow: ' + d.scalar('CompInflow'))
    w('  InflowFile: ' + _as_str(d.scalar('InflowFile')))
    w('  HWindSpeed: ' + d.scalar('HWindSpeed'))
    w('  RefHt: ' + d.scalar('RefHt'))
    w('  PLExp: ' + d.scalar('PLExp'))
    w('')

    w('seastate:')
    w('  CompSeaSt: ' + d.scalar('CompSeaSt'))
    w('  SeaStFile: ' + _as_str(d.scalar('SeaStFile')))
    w('')

    n_turbines = int(d.scalar('NumTurbines'))
    w('turbines:')
    for iwt in range(1, n_turbines + 1):
        for line in _ad_driver_turbine_lines(d, iwt):
            w(line)
    w('')

    w('time_dependent_analysis:')
    w('  TimeAnalysisFileName: ' + _as_str(d.scalar('TimeAnalysisFileName')))
    w('')

    n_cases = int(d.scalar('NumCases'))
    CASE_KEYS = ('HWindSpeed', 'PLExp', 'rotSpeed', 'bldPitch', 'nacYaw', 'dT', 'tMax', 'DOF', 'amplitude', 'frequency')
    case_rows = _matrix_rows(d, n_cases, 10, skip=2)
    w('combined_case_analysis:')
    if n_cases > 0:
        w('  cases:')
        for row in case_rows:
            w(_row_map(CASE_KEYS, row))
    else:
        w('  cases: []')
    w('')

    w('outputs:')
    w('  outFmt: ' + _as_str(d.scalar('OutFmt')))
    w('  outFileFmt: ' + d.scalar('OutFileFmt'))
    w('  WrVTK: ' + d.scalar('WrVTK'))
    w('  WrVTK_Type: ' + d.scalar('WrVTK_Type'))
    w('  VTKHubRad: ' + d.scalar('VTKHubRad'))
    vtk_nac_dim = _flatten_csv(d.find('VTKNacDim'))[:6]
    w('  VTKNacDim: [' + _list_join(vtk_nac_dim) + ']')
    w('')

    return '\n'.join(out)


#-------------------------------- MoorDyn ---------------------------------------------

# The word-separator set of NWTC_IO's CountWords/GetWords (space, tab, comma, semicolon,
# single quote, double quote), which is also how MoorDyn's list-directed table READs
# tokenize a row.
_MD_SEPS = re.compile(r"[ \t,;'\"]+")

# A token safe to emit as a bare YAML plain scalar: only [A-Za-z0-9_.+-] characters and
# at least one alphanumeric (so "-", "|1e8", "SYROPE:f.dat" etc. get quoted instead).
_MD_PLAIN = re.compile(r'^(?=.*[A-Za-z0-9])[A-Za-z0-9_.+\-]+$')


def _md_cell(tok):
    """Emit one MoorDyn table cell verbatim: bare when it is a safe plain scalar,
    double-quoted otherwise (bar-separated multi-values, SYROPE:<file> prefixes, the
    lone "-" output-flag placeholder, ...). MoorDyn_Yaml.f90 reads every cell back as
    raw scalar text and re-emits it into the materialized row unchanged."""
    return tok if _MD_PLAIN.match(tok) else _as_str(tok)


def _md_words(line):
    """Tokenize like CountWords/GetWords: split on the shared separator set; quoted
    strings lose their quotes exactly as list-directed READ strips them."""
    return [w for w in _MD_SEPS.split(line) if w]


def convert_moordyn(text_path):
    """Convert a text-format MoorDyn primary input file to its YAML schema
    (modules/moordyn/src/MoorDyn_Yaml.f90 is the source of truth: the YAML reader
    materializes the canonical text deck back for MD_Init's free-form walker, every
    cell verbatim).

    The text format is free-form: "---" section headers found by case-insensitive
    substring match (mirrored here in MD_Init's own branch order), two skipped
    label/units lines per table, rows until the next header. All numeric literals,
    keyword cells (attachments, "seabed" depths, bar-separated multi-values,
    SYROPE:/lookup-table filenames, output-flag character sets) pass through
    verbatim as strings. Options keep their original keyword spellings (aliases and
    unknown keywords included) and document order; sub-files (bathymetry grids,
    WaterKin, lookup tables, Syrope working curves) stay referenced by path."""

    # mirror ProcessComFile: strip inline comments ('!', '#', '%'), drop lines that
    # are blank or comment-only -- the walker only ever sees this processed stream
    with open(text_path, 'r', errors='replace') as f:
        raw = f.read().splitlines()
    lines = []
    for line in raw:
        if _is_comment_or_blank(line):
            continue
        s = _strip_inline_comment(line).strip()
        if s:
            lines.append(s)

    line_types, rod_types, bodies, rods, points, md_lines = [], [], [], [], [], []
    syrope_ic, ext_loads, control, failure = [], [], [], []
    options = []   # (keyword, value) in document order
    channels = []

    n = len(lines)

    def collect_table(start):
        """Skip the two label/units lines after the header at `start`, then collect
        raw rows until the next '---' header (or EOF); return (rows, next_index)."""
        i = start + 3   # header + 2 label/units lines
        rows = []
        while i < n and '---' not in lines[i]:
            rows.append(lines[i])
            i += 1
        return rows, i

    i = 0
    while i < n:
        u = lines[i].upper()
        if '---' not in u:
            i += 1
            continue
        # branch order mirrors MD_Init's own header-matching chain
        if 'LINE DICTIONARY' in u or 'LINE TYPES' in u:
            rows, i = collect_table(i)
            for r in rows:
                toks = _md_words(r)
                if len(toks) not in (10, 11, 13):
                    raise ValueError('convert_moordyn: LINE TYPES row must have 10, 11, or 13 '
                                     'columns (got {}): "{}"'.format(len(toks), r))
                line_types.append(toks)
        elif 'ROD DICTIONARY' in u or 'ROD TYPES' in u:
            rows, i = collect_table(i)
            for r in rows:
                toks = _md_words(r)
                if len(toks) < 7:
                    raise ValueError('convert_moordyn: ROD TYPES row must have at least 7 '
                                     'columns: "{}"'.format(r))
                rod_types.append(toks[:7])   # the walker READs 7; trailing extras are ignored
        elif 'BODIES' in u or 'BODY LIST' in u or 'BODY PROPERTIES' in u:
            rows, i = collect_table(i)
            for r in rows:
                toks = _md_words(r)
                if len(toks) != 14:
                    raise ValueError('convert_moordyn: BODIES row must have 14 columns '
                                     '(got {}): "{}"'.format(len(toks), r))
                bodies.append(toks)
        elif 'RODS' in u or 'ROD LIST' in u or 'ROD PROPERTIES' in u:
            rows, i = collect_table(i)
            for r in rows:
                toks = _md_words(r)
                if len(toks) != 11:
                    raise ValueError('convert_moordyn: RODS row must have 11 columns '
                                     '(got {}): "{}"'.format(len(toks), r))
                rods.append(toks)
        elif ('POINTS' in u or 'CONNECTION PROPERTIES' in u or 'NODE PROPERTIES' in u
              or 'POINT PROPERTIES' in u or 'POINT LIST' in u):
            rows, i = collect_table(i)
            for r in rows:
                toks = _md_words(r)
                if len(toks) != 9:
                    raise ValueError('convert_moordyn: POINTS row must have 9 columns '
                                     '(got {}): "{}"'.format(len(toks), r))
                points.append(toks)
        elif 'LINES' in u or 'LINE PROPERTIES' in u or 'LINE LIST' in u:
            rows, i = collect_table(i)
            for r in rows:
                toks = _md_words(r)
                if len(toks) < 7:
                    raise ValueError('convert_moordyn: LINES row must have at least 7 '
                                     'columns: "{}"'.format(r))
                md_lines.append(toks[:7])   # the walker READs 7; trailing extras are ignored
        elif 'SYROPE IC' in u:
            rows, i = collect_table(i)
            for r in rows:
                nids = r.count(',') + 1     # the walker derives the ID count from commas
                toks = _md_words(r)
                if len(toks) != nids + 2:
                    raise ValueError('convert_moordyn: SYROPE IC row must be <lineIDs>, '
                                     'Tmax, Tmean: "{}"'.format(r))
                syrope_ic.append((toks[:nids], toks[nids], toks[nids+1]))
        elif 'EXTERNAL LOADS' in u:
            rows, i = collect_table(i)
            for r in rows:
                toks = _md_words(r)
                if len(toks) != 6:
                    raise ValueError('convert_moordyn: EXTERNAL LOADS row must have 6 '
                                     'columns (got {}): "{}"'.format(len(toks), r))
                ext_loads.append(toks)
        elif 'CONTROL' in u:
            rows, i = collect_table(i)
            for r in rows:
                nids = r.count(',') + 1
                toks = _md_words(r)
                if len(toks) != nids + 1:
                    raise ValueError('convert_moordyn: CONTROL row must be CtrlChan, '
                                     '<lineIDs>: "{}"'.format(r))
                control.append((toks[0], toks[1:1+nids]))
        elif 'FAILURE' in u:
            rows, i = collect_table(i)
            for r in rows:
                nids = r.count(',') + 1
                toks = _md_words(r)
                if len(toks) != nids + 4:
                    raise ValueError('convert_moordyn: FAILURE row must be FailID, Point, '
                                     '<lineIDs>, FailTime, FailTen: "{}"'.format(r))
                failure.append((toks[0], toks[1], toks[2:2+nids], toks[2+nids], toks[3+nids]))
        elif 'OPTIONS' in u:
            i += 1   # no label/units lines in the options section
            while i < n and '---' not in lines[i]:
                toks = _md_words(lines[i])
                if len(toks) < 2:
                    raise ValueError('convert_moordyn: OPTIONS line needs "<value> '
                                     '<keyword>": "{}"'.format(lines[i]))
                if any(k.lower() == toks[1].lower() for k, _ in options):
                    raise ValueError('convert_moordyn: duplicate option keyword "{}" cannot '
                                     'be represented as a YAML mapping'.format(toks[1]))
                options.append((toks[1], toks[0]))
                i += 1
        elif 'OUTPUT' in u:
            i += 1
            while i < n:
                s = lines[i]
                su = s.upper()
                if '---' in su or 'END' in su:
                    break
                # mirror the walker's leading-quote handling: a quoted channel list
                # keeps only the quoted content (anything after the close quote is a
                # comment)
                if s[0] in '\'"':
                    close = s.find(s[0], 1)
                    if close > 0:
                        s = s[:close+1]
                channels.extend(_md_words(s))
                i += 1
        else:
            i += 1   # unrecognized header: skipped silently, exactly like the walker

    out = []
    w = out.append
    w('# MoorDyn primary input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    if line_types:
        w('line_types:')
        base_keys = ['Name', 'Diam', 'MassDen', 'EA', 'BA', 'EI', 'Cd', 'Ca', 'CdAx', 'CaAx']
        for toks in line_types:
            keys = list(base_keys)
            if len(toks) >= 11:
                keys.append('Cl')
            if len(toks) == 13:
                keys += ['dF', 'cF']
            w(_row_map(keys, [_md_cell(t) for t in toks]))
    if rod_types:
        w('rod_types:')
        keys = ['Name', 'Diam', 'MassDen', 'Cd', 'Ca', 'CdEnd', 'CaEnd']
        for toks in rod_types:
            w(_row_map(keys, [_md_cell(t) for t in toks]))
    if bodies:
        w('bodies:')
        keys = ['ID', 'Attachment', 'X0', 'Y0', 'Z0', 'r0', 'p0', 'y0', 'M', 'CG', 'I', 'V', 'CdA', 'Ca']
        for toks in bodies:
            w(_row_map(keys, [_md_cell(t) for t in toks]))
    if rods:
        w('rods:')
        keys = ['ID', 'RodType', 'Attachment', 'Xa', 'Ya', 'Za', 'Xb', 'Yb', 'Zb', 'NumSegs', 'Outputs']
        for toks in rods:
            w(_row_map(keys, [_md_cell(t) for t in toks]))
    if points:
        w('points:')
        keys = ['ID', 'Attachment', 'X', 'Y', 'Z', 'M', 'V', 'CdA', 'Ca']
        for toks in points:
            w(_row_map(keys, [_md_cell(t) for t in toks]))
    if md_lines:
        w('lines:')
        keys = ['ID', 'LineType', 'AttachA', 'AttachB', 'UnstrLen', 'NumSegs', 'Outputs']
        for toks in md_lines:
            w(_row_map(keys, [_md_cell(t) for t in toks]))
    if syrope_ic:
        w('syrope_ic:')
        for ids, tmax, tmean in syrope_ic:
            w('    - Lines: [' + ', '.join(ids) + ']')
            w('      Tmax: ' + _md_cell(tmax))
            w('      Tmean: ' + _md_cell(tmean))
    if ext_loads:
        w('external_loads:')
        keys = ['ID', 'Object', 'Fext', 'Blin', 'Bquad', 'CSys']
        for toks in ext_loads:
            w(_row_map(keys, [_md_cell(t) for t in toks]))
    if control:
        w('control:')
        for chan, ids in control:
            w('    - ChannelID: ' + _md_cell(chan))
            w('      Lines: [' + ', '.join(ids) + ']')
    if failure:
        w('failure:')
        for fid, attach, ids, ftime, ften in failure:
            w('    - ID: ' + _md_cell(fid))
            w('      Attachment: ' + _md_cell(attach))
            w('      Lines: [' + ', '.join(ids) + ']')
            w('      FailTime: ' + _md_cell(ftime))
            w('      FailTen: ' + _md_cell(ften))
    if options:
        w('options:')
        for key, val in options:
            w('  {}: {}'.format(key, _md_cell(val)))
    if channels:
        w('outputs:')
        w('  OutList:')
        for c in channels:
            w('    - ' + c)
    w('')

    return '\n'.join(out)


def convert_moordyn_driver(text_path):
    """Convert a text-format MoorDyn driver input file (Wave 4, class-B/sequential-
    reader driver -- follows the seastate/subdyn convention) to its YAML schema
    (modules/moordyn/src/MoorDyn_Driver_Yaml.f90 is the source of truth).

    Schema mirrors the driver's text reader (MoorDyn_Driver.f90:755-841,
    ReadDriverInputFile) key-for-key:
        environmental_conditions:  Gravity, rhoW, WtrDpth
        moordyn:                   MDInputFile, OutRootName, TMax, dtC
        inputs:                    InputsMod, InputsFile
        farm:                      NumTurbines, SeaStateFile (optional, default "" --
                                    a modern-format concession, no legacy warning),
                                    initial_positions (list of row mappings keyed by
                                    the text table's own column labels: ref_X, ref_Y,
                                    surge_init, sway_init, heave_init, roll_init,
                                    pitch_init, yaw_init). Its length must be exactly
                                    MAX(1, NumTurbines) rows, mirroring the text
                                    path's `do J=1,MAX(1,InitInp%FarmSize)` loop.

    Unlike SeaState/SubDyn's drivers, MoorDyn's driver never reads an Echo flag at all
    ("echo is no longer used by MD", MoorDyn_Driver.f90:138), so there is no
    general:Echo key here.

    MDInputFile/OutRootName/InputsFile stay path-valued (second-order rule;
    MDInputFile is NOT repointed at a .yaml sibling here -- that repointing, when
    needed, is done by the caller after this converter returns, mirroring the
    seastate/subdyn driver-mode harness's own convention). SeaStateFile, when present
    in the text file, is likewise emitted verbatim as a path; when absent the text
    reader's own '---' sentinel is detected here and the key is omitted (Default=''
    in MoorDyn_Driver_Yaml.f90 supplies the same empty value on read-back)."""
    d = _TextDeck(text_path)

    out = []
    w = out.append
    w('# MoorDyn driver input file (YAML form)')
    w('# converted from {} by yamlDeckConverter.py'.format(os.path.basename(text_path)))

    w('environmental_conditions:')
    w('  Gravity: ' + d.scalar('Gravity'))
    w('  rhoW: ' + d.scalar('rhoW'))
    w('  WtrDpth: ' + d.scalar('WtrDpth'))
    w('')

    w('moordyn:')
    w('  MDInputFile: ' + _as_str(d.scalar('MDInputFile')))
    w('  OutRootName: ' + _as_str(d.scalar('OutRootName')))
    w('  TMax: ' + d.scalar('TMax'))
    w('  dtC: ' + d.scalar('dtC'))
    w('')

    w('inputs:')
    w('  InputsMod: ' + d.scalar('InputsMode'))
    w('  InputsFile: ' + _as_str(d.scalar('InputsFile')))
    w('')

    n_turbines = int(d.scalar('NumTurbines'))

    # Peek the next significant line (MoorDyn_Driver.f90:802-810): if it is the
    # "---- Initial Positions ----" banner, no SeaStateFile was given (the text
    # reader's ReadVar consumes -- and discards -- that banner line as if it were the
    # SeaStateFile value); otherwise it is the SeaStateFile filename, and one further
    # (banner) line follows before the initial-positions table's two header lines.
    peek_idx = d.cursor
    while peek_idx < len(d.lines) and _is_comment_or_blank(d.lines[peek_idx]):
        peek_idx += 1
    peek_text = _strip_inline_comment(d.lines[peek_idx]).strip()
    if '---' in peek_text:
        sea_state_file = None
        d.cursor = peek_idx + 1   # consume the banner line, mirroring the text reader
        header_skip = 2           # just the table's own 2 header lines remain
    else:
        sea_state_file = d.scalar('SeaStateFile')
        header_skip = 3           # 1 extra banner line + the table's own 2 header lines

    w('farm:')
    w('  NumTurbines: ' + str(n_turbines))
    if sea_state_file is not None:
        w('  SeaStateFile: ' + _as_str(sea_state_file))

    n_rows = max(1, n_turbines)
    rows = d.table_rows(n_rows, skip=header_skip)
    POS_KEYS = ('ref_X', 'ref_Y', 'surge_init', 'sway_init', 'heave_init', 'roll_init', 'pitch_init', 'yaw_init')
    w('  initial_positions:')
    for raw in rows:
        toks = _row_tokens(raw)
        if len(toks) != 8:
            raise ValueError('convert_moordyn_driver: initial-positions row "{}" in {} has {} value(s); expected 8'.format(
                raw, text_path, len(toks)))
        w(_row_map(POS_KEYS, toks))
    w('')

    return '\n'.join(out)


#---------------------- single-file mode: inline relative-path rewrite ----------------
#
# Known converter caveat (found in 2.1): a module's own relative paths are always
# *written* relative to the module file's own directory (matching the runtime
# semantic: a text-format module file's relative paths resolve against that file, not
# against the .fst). In --single-file mode the module's converted document is embedded
# directly under the .fst's own document, and the documented single-file semantic is
# that paths inside an inlined section resolve relative to the *deck* instead (see
# docs/source/user/yaml_input.rst). So whenever the module file's own directory differs
# from the .fst's, every relative path inside the inlined section must be rewritten --
# prefixed with the relative directory from the deck to the module file's original
# location -- so it keeps resolving to the same file (the 5MW cases, whose InflowWind
# file lives in a shared ../5MW_Baseline/ directory while referencing its own
# Wind/*.bts file relative to itself, are the canonical exerciser). When the module
# file already lives alongside the .fst (the common case), the prefix is empty and
# this is a no-op.
#
# Per-module path-key tables: each entry is "section.Key" (the converter's top-level
# YAML section, then the scalar- or list-valued key within it) for the fixed-schema
# converters, or "options.OptionName" for MoorDyn's free-form OPTIONS keywords
# (matched case-insensitively, since MoorDyn option keyword case follows the original
# text file). Modules with no path-typed keys (Simplified ElastoDyn, AeroDisk) are
# simply absent.
_INLINE_PATH_KEYS = {
    'inflowwind': ['uniform_wind.FileName_Uni', 'turbsim_wind.FileName_BTS',
                   'bladed_wind.FilenameRoot', 'hawc_wind.FileName_u',
                   'hawc_wind.FileName_v', 'hawc_wind.FileName_w'],
    'elastodyn':  ['blade.BldFile', 'furling.FurlFile', 'tower.TwrFile'],
    'aerodyn':    ['general.AA_InputFile', 'olaf_options.OLAFInputFileName',
                   'airfoil_info.AFNames', 'rotor_blade_properties.ADBlFile',
                   'tail_fin_aerodynamics.TFinFile'],
    'servodyn':   ['bladed_interface.DLL_FileName', 'bladed_interface.DLL_InFile',
                   'structural_control.BStCfiles', 'structural_control.NStCfiles',
                   'structural_control.TStCfiles', 'structural_control.SStCfiles'],
    'seastate':   ['waves.WvKinFile'],
    'hydrodyn':   ['floating_platform.PotFile', 'floating_platform.GeoFile'],
    'subdyn':     ['base_reaction_joints.SSIfile'],
    'extptfm':    ['reduction_inputs.RedFile', 'connections.ConnFile',
                   'user_forcing.ForceFile', 'user_forcing.FConnFile'],
    'moordyn':    ['options.WaterKin'],
    # note: MoorDyn's variant "depth" option (a number OR a bathymetry filename --
    # MoorDyn_IO.f90 MDIO_getBathymetry) is intentionally not listed above:
    # distinguishing the two forms needs content sniffing beyond a simple keyword
    # lookup, and no current case inlines a MoorDyn deck whose directory differs from
    # its .fst's; revisit the day one does.
}

# path-like values that are never real relative paths -- left untouched by the rewrite
_PLACEHOLDER_PATHS = ('unused', 'default', 'none', '')


def _relpath_prefix(from_dir, to_dir):
    """Directory (forward-slashed) to prepend to a path that was written relative to
    `from_dir` so it stays correct when interpreted relative to `to_dir` instead.
    Empty when the two directories are the same -- the common case, and the only case
    for any module file that lives alongside the .fst."""
    rel = os.path.relpath(os.path.abspath(from_dir), os.path.abspath(to_dir))
    return '' if rel in ('.', '') else rel.replace(os.sep, '/')


def _rewrite_path_token(value, prefix):
    """Prefix one genuine relative path with `prefix`; leave placeholders
    ("unused"/"default"/"none"/empty) and already-absolute paths untouched."""
    if not prefix or not value:
        return value
    if value.strip().lower() in _PLACEHOLDER_PATHS or os.path.isabs(value):
        return value
    return prefix + '/' + value


def _rewrite_inline_paths(yaml_text, module_key, prefix):
    """Rewrite the relative-path values of `module_key`'s known path-typed keys (see
    _INLINE_PATH_KEYS) found in `yaml_text`'s top-level sections, prefixing genuine
    relative paths with `prefix` (see _relpath_prefix). No-op when `prefix` is empty
    (module and deck already share a directory) or the module has no path-typed keys.

    Handles the three value shapes the converters emit: a double-quoted scalar
    (`Key: "path"`), a flow list of double-quoted scalars (`Key: ["a", "b"]` -- always
    one physical line for every path-typed list key emitted above), and MoorDyn's bare
    (unquoted) plain-scalar option cells (`WaterKin: WaterKin.dat`)."""
    path_keys = _INLINE_PATH_KEYS.get(module_key, [])
    if not prefix or not path_keys:
        return yaml_text

    by_section = {}
    for pk in path_keys:
        section, _, key = pk.rpartition('.')
        by_section.setdefault(section, set()).add(key.lower())

    quoted = re.compile(r'"((?:[^"\\]|\\.)*)"')

    def rewrite_quoted(m):
        raw = m.group(1).replace('\\"', '"').replace('\\\\', '\\')
        new_raw = _rewrite_path_token(raw, prefix)
        if new_raw == raw:
            return m.group(0)
        return '"' + new_raw.replace('\\', '\\\\').replace('"', '\\"') + '"'

    section = None
    out_lines = []
    for line in yaml_text.split('\n'):
        stripped = line.strip()
        if stripped and stripped.endswith(':') and not line[:1].isspace():
            section = stripped[:-1]
            out_lines.append(line)
            continue
        m = re.match(r'^(\s*)([A-Za-z_][A-Za-z0-9_]*):\s*(.*)$', line)
        if m and section in by_section and m.group(2).lower() in by_section[section]:
            indent, key, rest = m.groups()
            rest_stripped = rest.strip()
            if rest_stripped.startswith('"') or rest_stripped.startswith('['):
                new_rest = quoted.sub(rewrite_quoted, rest)
            elif rest_stripped:
                # MoorDyn's bare option cells: rewrite, then re-quote if the rewritten
                # value is no longer a safe bare scalar (_MD_PLAIN excludes '/')
                new_val = _rewrite_path_token(rest_stripped, prefix)
                if new_val != rest_stripped and not _MD_PLAIN.match(new_val):
                    new_val = _as_str(new_val)
                new_rest = new_val
            else:
                new_rest = rest
            out_lines.append('{}{}: {}'.format(indent, key, new_rest))
        else:
            out_lines.append(line)
    return '\n'.join(out_lines)


def _emit_ed_file_lines(ed_rel, comp_elast, mode, base_dir, extra_files, key_indent):
    """Return the output line(s) for a single EDFile entry -- shared by the primary
    rotor and each additional multirotor rotor (reg_tests/CTestList.cmake's
    MHK_RM1_Floating_MR case), so rotor 2+'s EDFile gets the same convert/inline
    treatment as rotor 1's in all-yaml/single-file mode instead of staying a bare
    text path. `ed_rel` must already be _unquote()'d. `key_indent` is the indent (in
    spaces) of the 'EDFile:' key itself (2 for the primary rotor's top-level
    input_files:, 4 for a '- EDFile:' list item under input_files:rotors); nested
    inline content is indented key_indent + 2, matching the primary-rotor pattern.
    """
    pad = ' ' * key_indent
    nested_pad = ' ' * (key_indent + 2)
    lines = []
    convert_sed_file = (comp_elast == 3) and (mode in ('all-yaml', 'single-file'))
    convert_ed_file  = (comp_elast in (1, 2)) and (mode in ('all-yaml', 'single-file'))
    if convert_sed_file:
        ed_abs = os.path.join(base_dir, ed_rel)
        sed_yaml_text = convert_sed(ed_abs)
        if mode == 'single-file':
            lines.append(pad + 'EDFile:')
            sed_lines = sed_yaml_text.split('\n')
            sed_body = '\n'.join(sed_lines[2:]) if len(sed_lines) > 2 else sed_yaml_text
            lines.append(_indent_block(sed_body, nested_pad))
        else:  # all-yaml
            sed_yaml_rel = os.path.splitext(ed_rel)[0] + '.yaml'
            extra_files[sed_yaml_rel] = sed_yaml_text
            lines.append(pad + 'EDFile: ' + _as_str(sed_yaml_rel))
    elif convert_ed_file:
        ed_abs = os.path.join(base_dir, ed_rel)
        ed_yaml_text = convert_elastodyn(ed_abs)
        if mode == 'single-file':
            lines.append(pad + 'EDFile:')
            # rewrite any relative paths inside the inlined section (BldFile/TwrFile/
            # FurlFile -- second-order files, still referenced by path) so they keep
            # resolving once nested under a deck at a different directory
            ed_yaml_text = _rewrite_inline_paths(
                ed_yaml_text, 'elastodyn', _relpath_prefix(os.path.dirname(ed_abs), base_dir))
            ed_lines = ed_yaml_text.split('\n')
            ed_body = '\n'.join(ed_lines[2:]) if len(ed_lines) > 2 else ed_yaml_text
            lines.append(_indent_block(ed_body, nested_pad))
        else:  # all-yaml
            ed_yaml_rel = os.path.splitext(ed_rel)[0] + '.yaml'
            extra_files[ed_yaml_rel] = ed_yaml_text
            lines.append(pad + 'EDFile: ' + _as_str(ed_yaml_rel))
    else:
        lines.append(pad + 'EDFile: ' + _as_str(ed_rel))
    return lines


def convert_fst(text_path, mode='per-file'):
    """Convert a text-format OpenFAST primary (.fst) input file to its YAML schema
    (modules/openfast-library/src/FAST_Yaml.f90 is the source of truth).

    mode:
      'per-file'    - all module input file entries stay as paths to the original
                      (text) files, unchanged.
      'all-yaml'    - same, but the referenced InflowWind file (if CompInflow == 1),
                      AeroDisk or AeroDyn file (if CompAero == 1 or 2 respectively;
                      AeroDyn's airfoil/blade/tailfin/AeroAcoustics/OLAF files stay
                      paths), EDFile (if CompElast == 1, 2, or 3 -- (Beam)ElastoDyn or
                      Simplified ElastoDyn; ElastoDyn's referenced BldFile/TwrFile/
                      FurlFile stay paths), ServoFile (if CompServo == 1, including its
                      referenced StC sub-files as their own .yaml file type), SeaStFile
                      (if CompSeaSt == 1), HydroFile (if CompHydro == 1; its
                      referenced PotFile/GeoFile potential-flow data stay paths),
                      and/or MooringFile (if CompMooring == 3, MoorDyn; its
                      bathymetry/WaterKin/lookup-table/Syrope sub-files stay paths) are
                      ALSO converted to YAML and their input_files entries are repointed
                      at the new .yaml files. Other module files stay as text paths.
      'single-file' - the InflowWind input (if CompInflow == 1), the AeroDisk or AeroDyn
                      input (if CompAero == 1 or 2 respectively), the EDFile input (if
                      CompElast == 1, 2, or 3; ElastoDyn's BldFile/TwrFile/FurlFile stay
                      referenced by path -- second-order files are never inlined), the
                      ServoFile input (if CompServo == 1; its StC
                      sub-files stay referenced by path -- StC input is never inlined),
                      the SeaStFile input (if CompSeaSt == 1), the HydroFile input
                      (if CompHydro == 1), and/or the MooringFile input (if
                      CompMooring == 3, MoorDyn) are inlined as a nested mapping under
                      input_files:InflowFile / input_files:AeroFile / input_files:EDFile
                      / input_files:ServoFile / input_files:SeaStFile /
                      input_files:HydroFile / input_files:MooringFile (the uniform
                      value rule: a mapping value is inline module input). Other
                      modules stay as text paths. Whenever an inlined module file's own
                      directory differs from the .fst's, relative paths inside its
                      inlined section (StC files, airfoil dirs, ADBlFile, wind files,
                      DLL_FileName, PotFile rootnames, ...) are rewritten relative to
                      the deck, per the documented single-file semantic (see
                      _rewrite_inline_paths / _INLINE_PATH_KEYS above).

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
    # both targets have a YAML reader now, so conversion/inlining is gated only on mode.
    # _emit_ed_file_lines() is shared with each additional multirotor rotor's EDFile
    # below, so rotor 2+ gets identical convert/inline treatment.
    ed_rel = _unquote(ed_file)
    for line in _emit_ed_file_lines(ed_rel, comp_elast, mode, base_dir, extra_files, key_indent=2):
        w(line)

    # BDBldFile (CompElast == 2): unlike EDFile, BeamDyn has no PassedFileIsYaml/
    # FileInfoType inline entry point at all (BeamDyn_Yaml.f90's InitInputType carries
    # only a plain file-path InputFile, in both text and YAML -- see FAST_Yaml.f90's
    # GetBDBldFiles, which only ever accepts a list of file paths). So BDBldFile entries
    # are never inlined as a nested mapping, even in single-file mode: in both all-yaml
    # and single-file modes each non-empty entry is converted to a sibling .yaml file
    # (same directory as the original, so BldFile's relative path inside it keeps
    # resolving unchanged) and BDBldFile is repointed at that path; perfile mode leaves
    # the original text paths untouched.
    convert_bd_files = (comp_elast == 2) and (mode in ('all-yaml', 'single-file'))
    bd_files_out = []
    for bd_tok in bd_files:
        bd_rel = _unquote(bd_tok)
        if convert_bd_files and bd_rel:
            bd_abs = os.path.join(base_dir, bd_rel)
            bd_yaml_text = convert_beamdyn(bd_abs)
            bd_yaml_rel = os.path.splitext(bd_rel)[0] + '.yaml'
            extra_files[bd_yaml_rel] = bd_yaml_text
            bd_files_out.append(_as_str(bd_yaml_rel))
        else:
            bd_files_out.append(_as_str(bd_rel))
    w('  BDBldFile: [' + ', '.join(bd_files_out) + ']')

    convert_inflow = (comp_inflow == 1) and (mode in ('all-yaml', 'single-file'))
    inflow_rel = _unquote(inflow_file_tok)
    if convert_inflow:
        inflow_abs = os.path.join(base_dir, inflow_rel)
        ifw_yaml_text = convert_inflowwind(inflow_abs)
        if mode == 'single-file':
            w('  InflowFile:')
            # rewrite any relative paths inside the inlined section (e.g. a shared
            # 5MW_Baseline/ InflowWind file's own "Wind/..." wind files) so they keep
            # resolving once nested under a deck at a different directory
            ifw_yaml_text = _rewrite_inline_paths(
                ifw_yaml_text, 'inflowwind', _relpath_prefix(os.path.dirname(inflow_abs), base_dir))
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

    convert_aerodisk_file = (comp_aero == 1) and (mode in ('all-yaml', 'single-file'))
    convert_aerodyn_file  = (comp_aero == 2) and (mode in ('all-yaml', 'single-file'))
    aero_rel = _unquote(aero_file)
    if convert_aerodisk_file:
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
    elif convert_aerodyn_file:
        aero_abs = os.path.join(base_dir, aero_rel)
        # AeroDyn's Hub/Nacelle/TailFin/Tower sections repeat once per rotor; NRotors
        # is already known here (read from the .fst above), so pass it through.
        # Likewise, the true blade count (needed to drop unused ADBlFile padding lines
        # for turbines with fewer than MaxBl=3 blades) comes from each rotor's own
        # ElastoDyn NumBl -- never from the AeroDyn primary file, exactly as the
        # Fortran glue code supplies it.
        num_blades_total = _ed_num_blades(os.path.join(base_dir, ed_rel))
        for (r_ed, _r_bd, _r_serv) in rotors_extra:
            num_blades_total += _ed_num_blades(os.path.join(base_dir, _unquote(r_ed)))
        ad_yaml_text = convert_aerodyn(aero_abs, n_rotors=n_rotors, num_blades_total=num_blades_total)
        if mode == 'single-file':
            w('  AeroFile:')
            # rewrite any relative paths inside the inlined section (airfoil dirs,
            # ADBlFile, tailfin) so they keep resolving once nested under a deck at a
            # different directory
            ad_yaml_text = _rewrite_inline_paths(
                ad_yaml_text, 'aerodyn', _relpath_prefix(os.path.dirname(aero_abs), base_dir))
            # drop the two leading '# ...' header comments before inlining, then
            # indent so the embedded document's top-level keys land under AeroFile:
            ad_lines = ad_yaml_text.split('\n')
            ad_body = '\n'.join(ad_lines[2:]) if len(ad_lines) > 2 else ad_yaml_text
            w(_indent_block(ad_body, '    '))
        else:  # all-yaml
            ad_yaml_rel = os.path.splitext(aero_rel)[0] + '.yaml'
            extra_files[ad_yaml_rel] = ad_yaml_text
            w('  AeroFile: ' + _as_str(ad_yaml_rel))
    else:
        w('  AeroFile: ' + _as_str(aero_rel))

    convert_servo = (comp_servo == 1) and (mode in ('all-yaml', 'single-file'))
    servo_rel = _unquote(servo_file)
    if convert_servo:
        servo_abs = os.path.join(base_dir, servo_rel)
        if mode == 'single-file':
            # inline the ServoDyn input; StC sub-files are a second-order file type and
            # always stay referenced by path (their relative paths keep resolving
            # because paths inside an inline section resolve relative to the deck)
            srvd_yaml_text, _ = convert_servodyn(servo_abs, stc_to_yaml=False)
            w('  ServoFile:')
            # rewrite any relative paths inside the inlined section (DLL_FileName/
            # DLL_InFile, StC file references) so they keep resolving once nested
            # under a deck at a different directory
            srvd_yaml_text = _rewrite_inline_paths(
                srvd_yaml_text, 'servodyn', _relpath_prefix(os.path.dirname(servo_abs), base_dir))
            # drop the two leading '# ...' header comments before inlining, then
            # indent so the embedded document's top-level keys land under ServoFile:
            srvd_lines = srvd_yaml_text.split('\n')
            srvd_body = '\n'.join(srvd_lines[2:]) if len(srvd_lines) > 2 else srvd_yaml_text
            w(_indent_block(srvd_body, '    '))
        else:  # all-yaml: also convert the referenced StC sub-files to their own
               # .yaml file type (still referenced by path from the ServoDyn deck)
            srvd_yaml_text, stc_files = convert_servodyn(servo_abs, stc_to_yaml=True)
            servo_dir_rel = os.path.dirname(servo_rel)
            for stc_rel, stc_text in stc_files.items():
                extra_files[os.path.join(servo_dir_rel, stc_rel) if servo_dir_rel else stc_rel] = stc_text
            servo_yaml_rel = os.path.splitext(servo_rel)[0] + '.yaml'
            extra_files[servo_yaml_rel] = srvd_yaml_text
            w('  ServoFile: ' + _as_str(servo_yaml_rel))
    else:
        w('  ServoFile: ' + _as_str(servo_rel))

    convert_seast = (comp_seast == 1) and (mode in ('all-yaml', 'single-file'))
    seast_rel = _unquote(seast_file)
    if convert_seast:
        seast_abs = os.path.join(base_dir, seast_rel)
        seast_yaml_text = convert_seastate(seast_abs)
        if mode == 'single-file':
            w('  SeaStFile:')
            # rewrite any relative paths inside the inlined section (WvKinFile) so
            # they keep resolving once nested under a deck at a different directory
            seast_yaml_text = _rewrite_inline_paths(
                seast_yaml_text, 'seastate', _relpath_prefix(os.path.dirname(seast_abs), base_dir))
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

    convert_hydro = (comp_hydro == 1) and (mode in ('all-yaml', 'single-file'))
    hydro_rel = _unquote(hydro_file)
    if convert_hydro:
        hydro_abs = os.path.join(base_dir, hydro_rel)
        hydro_yaml_text = convert_hydrodyn(hydro_abs)
        if mode == 'single-file':
            w('  HydroFile:')
            # rewrite any relative paths inside the inlined section (PotFile/GeoFile
            # rootnames) so they keep resolving once nested under a deck at a
            # different directory
            hydro_yaml_text = _rewrite_inline_paths(
                hydro_yaml_text, 'hydrodyn', _relpath_prefix(os.path.dirname(hydro_abs), base_dir))
            # drop the two leading '# ...' header comments before inlining, then
            # indent so the embedded document's top-level keys land under HydroFile:
            hydro_lines = hydro_yaml_text.split('\n')
            hydro_body = '\n'.join(hydro_lines[2:]) if len(hydro_lines) > 2 else hydro_yaml_text
            w(_indent_block(hydro_body, '    '))
        else:  # all-yaml
            hydro_yaml_rel = os.path.splitext(hydro_rel)[0] + '.yaml'
            extra_files[hydro_yaml_rel] = hydro_yaml_text
            w('  HydroFile: ' + _as_str(hydro_yaml_rel))
    else:
        w('  HydroFile: ' + _as_str(hydro_rel))

    # SubFile conversion/inlining is gated on CompSub == 1 (SubDyn) or CompSub == 2
    # (ExtPtfm_MCKF) -- both have a YAML reader now (SubDyn_Yaml.f90 / ExtPtfm_Yaml.f90,
    # and FAST_Yaml.f90's InlineTarget='SubDyn'/'ExtPtfm' gating). Any other CompSub value
    # keeps SubFile a plain (text) path in every mode.
    if comp_sub == 1:
        sub_module_key = 'subdyn'
        sub_converter = convert_subdyn
    elif comp_sub == 2:
        sub_module_key = 'extptfm'
        sub_converter = convert_extptfm
    else:
        sub_module_key = None
        sub_converter = None
    convert_sub = (sub_converter is not None) and (mode in ('all-yaml', 'single-file'))
    sub_rel = _unquote(sub_file)
    if convert_sub:
        sub_abs = os.path.join(base_dir, sub_rel)
        sub_yaml_text = sub_converter(sub_abs)
        if mode == 'single-file':
            w('  SubFile:')
            # rewrite any relative paths inside the inlined section (SubDyn: a reaction
            # joint's SSIfile; ExtPtfm: RedFile/ConnFile/ForceFile/FConnFile) so they keep
            # resolving once nested under a deck at a different directory
            sub_yaml_text = _rewrite_inline_paths(
                sub_yaml_text, sub_module_key, _relpath_prefix(os.path.dirname(sub_abs), base_dir))
            # drop the two leading '# ...' header comments before inlining, then
            # indent so the embedded document's top-level keys land under SubFile:
            sub_lines = sub_yaml_text.split('\n')
            sub_body = '\n'.join(sub_lines[2:]) if len(sub_lines) > 2 else sub_yaml_text
            w(_indent_block(sub_body, '    '))
        else:  # all-yaml
            sub_yaml_rel = os.path.splitext(sub_rel)[0] + '.yaml'
            extra_files[sub_yaml_rel] = sub_yaml_text
            w('  SubFile: ' + _as_str(sub_yaml_rel))
    else:
        w('  SubFile: ' + _as_str(sub_rel))

    # MooringFile conversion/inlining is gated on CompMooring == 3 (MoorDyn) or
    # CompMooring == 2 (FEAMooring) -- both have a YAML reader now (MoorDyn_IO.f90's
    # reader / FEAM_Yaml.f90, and FAST_Yaml.f90's InlineTarget='MoorDyn'/'FEAMooring'
    # gating). MAP++ (=1) and OrcaFlex (=4) files stay text paths in every mode.
    if comp_mooring == 3:
        mooring_module_key = 'moordyn'
        mooring_converter = convert_moordyn
    elif comp_mooring == 2:
        mooring_module_key = 'feamooring'
        mooring_converter = convert_feamooring
    else:
        mooring_module_key = None
        mooring_converter = None
    convert_mooring = (mooring_converter is not None) and (mode in ('all-yaml', 'single-file'))
    mooring_rel = _unquote(mooring_file)
    if convert_mooring:
        mooring_abs = os.path.join(base_dir, mooring_rel)
        md_yaml_text = mooring_converter(mooring_abs)
        if mode == 'single-file':
            w('  MooringFile:')
            # rewrite any relative paths inside the inlined section (MoorDyn: a WaterKin
            # bathymetry/water-kinematics file; FEAMooring has none) so they keep
            # resolving once nested under a deck at a different directory
            md_yaml_text = _rewrite_inline_paths(
                md_yaml_text, mooring_module_key, _relpath_prefix(os.path.dirname(mooring_abs), base_dir))
            # drop the two leading '# ...' header comments before inlining, then
            # indent so the embedded document's top-level keys land under MooringFile:
            md_doc_lines = md_yaml_text.split('\n')
            md_body = '\n'.join(md_doc_lines[2:]) if len(md_doc_lines) > 2 else md_yaml_text
            w(_indent_block(md_body, '    '))
        else:  # all-yaml
            md_yaml_rel = os.path.splitext(mooring_rel)[0] + '.yaml'
            extra_files[md_yaml_rel] = md_yaml_text
            w('  MooringFile: ' + _as_str(md_yaml_rel))
    else:
        w('  MooringFile: ' + _as_str(mooring_file))

    # IceFile conversion/inlining is gated on CompIce == 2 (IceDyn) -- it has a YAML
    # reader now (IceDyn_Yaml.f90, and FAST_Yaml.f90's InlineTarget='IceDyn' gating).
    # IceFloe (=1) has no YAML schema yet (that's a separate task) and keeps IceFile a
    # plain (text) path in every mode.
    convert_ice = (comp_ice == 2) and (mode in ('all-yaml', 'single-file'))
    ice_rel = _unquote(ice_file)
    if convert_ice:
        ice_abs = os.path.join(base_dir, ice_rel)
        iced_yaml_text = convert_icedyn(ice_abs)
        if mode == 'single-file':
            w('  IceFile:')
            # IceDyn references no further files, so no inline-path rewriting is needed;
            # drop the two leading '# ...' header comments before inlining, then indent
            # so the embedded document's top-level keys land under IceFile:
            iced_lines = iced_yaml_text.split('\n')
            iced_body = '\n'.join(iced_lines[2:]) if len(iced_lines) > 2 else iced_yaml_text
            w(_indent_block(iced_body, '    '))
        else:  # all-yaml
            iced_yaml_rel = os.path.splitext(ice_rel)[0] + '.yaml'
            extra_files[iced_yaml_rel] = iced_yaml_text
            w('  IceFile: ' + _as_str(iced_yaml_rel))
    else:
        w('  IceFile: ' + _as_str(ice_file))
    w('  SoilFile: '    + _as_str(soil_file))

    if rotors_extra:
        w('  rotors:')
        # additional rotors' EDFile: converted to YAML in all-yaml/single-file mode
        # (never left a bare text path), same as rotor 1's -- but never *inlined* as a
        # nested mapping even in single-file mode. Unlike rotor 1's input_files:EDFile
        # (GetModFile in FAST_Yaml.f90), the per-rotor sequence entry is read by
        # GetRotorFile, which only accepts a scalar file path (see its "plain path
        # only" doc comment) -- it has no mapping/InlineTarget branch, so a genuine
        # inline mapping here would be a Fortran-side parse error. Routing extra
        # rotors' EDFile through 'all-yaml' handling regardless of the requested mode
        # keeps every mode converter-only and still exercises the conversion path.
        rotor_ed_mode = 'all-yaml' if mode == 'single-file' else mode
        for (r_ed, r_bd, r_serv) in rotors_extra:
            r_ed_rel = _unquote(r_ed)
            ed_lines = _emit_ed_file_lines(r_ed_rel, comp_elast, rotor_ed_mode, base_dir, extra_files, key_indent=4)
            # ed_lines[0] is 4-space-indented ('    EDFile: ...'); turn it into the
            # sequence-item form '  - EDFile: ...' (dash + SPACE, so YAML reads it as a
            # sequence entry, not a '-EDFile' scalar key). The continuation keys below
            # keep the 4-space indent, aligning their column with EDFile's (column 5).
            w('  - ' + ed_lines[0][len('    '):])
            for extra_line in ed_lines[1:]:
                w(extra_line)
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
