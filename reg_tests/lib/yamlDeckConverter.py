"""
    Standalone text -> YAML converters for OpenFAST input decks, used by the
    YAML-equivalence regression tests (executeYamlEquivalenceCase.py).

    Deliberately dependency-free (no openfast_io): each converter knows one module's
    text layout and emits the documented YAML schema for it. Numeric and boolean
    values are copied as their original literal text so that Fortran's list-directed
    READ produces bit-identical values in both formats.

    Converters:
        convert_inflowwind(text_path) -> str   (YAML document)
"""

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
    return '"' + _unquote(tok) + '"'


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
