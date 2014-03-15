import logging
import unittest

__author__ = "Indika Piyasena"

logger = logging.getLogger(__name__)


class Sums:
    def __init__(self):
        pass

    def sums(self):
        source = range(1000)
        r = [x for x in source if x % 3 == 0 or x % 5 == 0]
        print sum(r)
        pass


class SumsTestCase(unittest.TestCase):
    def setUp(self):
        self.sums = Sums()

    def test_sums(self):
        self.sums.sums()


if __name__ == '__main__':
    unittest.main()
