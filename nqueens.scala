#!/usr/bin/env scala

import Math._

trait Piece {
    def nonattacking(other: Queen): Boolean
}

case class Queen(x: Int, y: Int) extends Piece {
    def nonattacking(other: Queen): Boolean = {
        x != other.x && y != other.y && abs( other.x - x ) != abs( other.y - y )
    }
}

// A chessboard with some number of Queens pieces placed
case class Solution(queens: List[Queen]) {
    val n = queens.length
    override def toString: String = {
        (0 to n - 1).map { i =>
            ((0 to n - 1).map { j =>
                (queens contains Queen(i, j)) match {
                    case true => "1 "
                    case false => "0 "
                }
            }).mkString("")
        }.mkString("\n")
    }
}

object Solution {
    // Chessboard's latest added Queen is not attacking any other Queens
    def promising(solution: Solution): Boolean = {
        solution.queens match {
            case head +: tail => tail.forall(_ nonattacking head)
        }
    }

    // Recursive backtracing to find a full Solution with n Queen pieces
    def backtracking(n: Int, solution: Solution): Solution = {
        solution.queens match {
            case hd::tail if (hd.x == n) => backtracking(n, Solution(Queen(tail.head.x + 1, tail.head.y) +: tail.tail))
            case hd::tail if promising(solution) => solution.queens.length match {
                case `n` => solution  // found a valid solution
                case _ => backtracking(n, Solution(Queen(0, hd.y + 1) +: solution.queens))
            }
            case hd::tail => backtracking(n, Solution(Queen(hd.x + 1, hd.y) +: tail))
        }
    }

    def findSolution(n: Int): Solution = {
        assert(n > 3)
        backtracking(n, Solution(List(Queen(0, 0))))
    }
}

// toString
val toStringSolution = Solution(List(Queen(0, 0), Queen(1, 1)))
assert("1 0 \n0 1 " == toStringSolution.toString)

// nonattacking
assert(Queen(1, 0) nonattacking Queen(2, 3))
assert(!( Queen(1, 0) nonattacking Queen(0, 1) ))

// promising
val solutionThatIsPromising = Solution(List(Queen(0, 0), Queen(2, 4)))
val solutionThatIsNotPromising = Solution(List(Queen(5, 5), Queen(2, 2)))
assert(Solution.promising(solutionThatIsPromising))
assert(!Solution.promising(solutionThatIsNotPromising))

// finding a solution
println(Solution.findSolution(args(0).toInt))


