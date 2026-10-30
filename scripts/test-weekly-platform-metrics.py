import copy
import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location('metrics', Path(__file__).with_name('weekly-platform-metrics.py'))
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)


class WeeklyMetrics(unittest.TestCase):
    def fixture(self):
        return {'schema_version': 1, 'source': 'postgresql-read-only-aggregate',
                'cutoff_utc': '2026-10-30T13:40:00+00:00', 'collected_at': '2026-10-30T14:00:00Z',
                'periods': {'current': dict(zip(m.COUNTS, [3, 2, 8])),
                            'previous': dict(zip(m.COUNTS, [0, 4, 6]))}}

    def test_equivalent_offsets_and_half_open_windows(self):
        sql = m.query('2026-10-30T08:40:00-05:00')
        self.assertEqual(sql, m.query('2026-10-30T13:40:00Z'))
        for stamp in ('2026-10-16T13:40:00', '2026-10-23T13:40:00', '2026-10-30T13:40:00'):
            self.assertIn(stamp, sql)
        self.assertIn('READ ONLY', sql)
        self.assertIn(' < end_at', sql)
        self.assertNotIn('primary_email', sql)

    def test_deltas_zero_denominator_and_unknowns(self):
        result = m.summarize(self.fixture(), '2026-10-30T08:40:00-05:00')
        self.assertIsNone(result['metrics']['recorded_signups']['percent_change'])
        self.assertEqual(result['metrics']['recorded_first_values']['percent_change'], -50)
        self.assertTrue(all(v is None for v in result['unavailable'].values()))
        self.assertEqual(result['cutoff'], '2026-10-30T08:40:00-05:00')

    def test_refuses_incomplete_negative_boolean_and_private_fields(self):
        changes = [lambda d: d['periods'].pop('previous'),
                   lambda d: d['periods']['current'].update(recorded_signups=-1),
                   lambda d: d['periods']['current'].update(recorded_signups=True),
                   lambda d: d.update(email='private@example.test'),
                   lambda d: d.update(collected_at='2026-10-29T14:00:00Z')]
        for change in changes:
            data = copy.deepcopy(self.fixture()); change(data)
            with self.assertRaises(ValueError):
                m.summarize(data, '2026-10-30T13:40:00Z')

    def test_refuses_stale_cutoff_and_injected_or_naive_dates(self):
        with self.assertRaises(ValueError):
            m.summarize(self.fixture(), '2026-10-31T13:40:00Z')
        for value in ('2026-10-30T08:40:00', "2026-10-30'; DROP TABLE party;--"):
            with self.assertRaises(ValueError): m.query(value)


if __name__ == '__main__':
    unittest.main()
