class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  
  def piece_size
    @all_rotations[0].size
  end

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new([[[0, 0]]], board)
  end
  # class array holding all the pieces and their rotations
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1, -1]]), # Fat L
               [[[0, 0], [-2, 0], [-1, 0], [1, 0], [2, 0]], # x-long
               [[0, 0], [0, -2], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, 1], [1, 0]])] # short L


end

class MyBoard < Board

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat_flag = false
  end

  def rotate_180
    rotate_clockwise
    rotate_clockwise
  end

  def next_piece
    if @cheat_flag
      @current_block = MyPiece.cheat_piece(self)
      @cheat_flag = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
    
  end
  
  def cheat
    if !game_over? and @game.is_running? and @score >= 100 and !@cheat_flag
      @score = @score - 100
      @cheat_flag = true
    end  
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(@current_block.piece_size-1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
    
  end

end
