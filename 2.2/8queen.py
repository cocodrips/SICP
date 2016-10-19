class QueenN():
    def __init__(self):
        pass

    def create(self, n):
        boards = [[-1] * n]
        for row in range(n):
            boards = self.add_row(row, n, boards)

        print n, "Pattern:", len(boards)
        # for board in boards:
        #     for col in board:
        #         for i in xrange(n):
        #             if i == col:
        #                 print "Q",
        #             else: 
        #                 print "_",
        #         print 
        #     print 


    def add_row(self, row, n, boards):
        next_boards = []
        for board in boards:
            for col in range(n):
                array = board[:]
                if self.is_safe(board, row, col):
                    array[row] = col
                    next_boards.append(array)

        return next_boards

    def is_safe(self, board, row, col):
        # cols
        if col in board:
            return False

        # x
        for i in range(row):
            if (i - board[i]) == (row - col):
                return False
            if abs(board[i] + i) == (row + col):
                return False
        return True


def enumerate_interval(_from, _to):
    return range(_from, _to)


def queen(board_size):
    def is_safe(k, positions):
        return True

    def empty_board():
        return []

    def adjoin_position(row, k, rest_queens):
        pass



    def queen_col(k):
        if k == 0:
            return []
        return [adjoin(row, k, rest_queens) for row in range(1, board_size)]


import unittest
class QueenTest(unittest.TestCase):
    def test_enumerate_interval(self):
        target = enumerate_interval(3, 5)
        self.assertTrue(target, range(3,5))

if __name__ == '__main__':
    # unittest.main()
    queen = QueenN()
    for i in range(3, 9):
        queen.create(i)
