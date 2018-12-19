package hw1;

import java.util.Arrays;

/**
 * State class for a chess game. Outside classes will read and update this state
 * as the game progresses.
 */
public class ChessGameState {
  private volatile boolean whoseTurn; // 0 for black, 1 for white.
  private volatile Piece[][] board;  // Current board state.
  private volatile Pair<Pair<Integer>> lastMove;  // Last move made.
  
  public ChessGameState() {
    board = new Piece[8][8];
  }
  
  public synchronized void update(Pair<Integer> start, Pair<Integer> end) {
    board[end.x][end.y] = board[start.x][start.y];
    board[start.x][start.y] = Piece.NONE;
    whoseTurn = !whoseTurn;
    lastMove = new Pair<Pair<Integer>>(start, end);
  }

  public synchronized boolean getWhoseTurn() {
	  return whoseTurn;
  }
  
  public synchronized Piece[][] getBoard() {
	  return copyBoard();
  }
  
  public synchronized Pair<Pair<Integer>> getLastMove() {
    return lastMove;
  }
  
  public static enum Piece {
    NONE, ROOK, KNIGHT, BISHOP, KING, QUEEN, PAWN;
  }
  
  public class Pair<T> {
    final T x, y;
    public Pair(T x, T y) {
      this.x = x;
      this.y = y;
    }
  }
  
  
  //Added this
  private Piece[][] copyBoard(){
  	Piece ret[][] = new Piece[board.length][];
  	for(int i = 0; i < board.length; i++){
  		ret[i] = Arrays.copyOf(board[i], board[i].length);
  	}
  	return ret;
  }
}
