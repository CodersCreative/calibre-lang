enum Player{
  None,
  X,
  O
}

impl Player {
  fn default() -> Player {
    return Player.None;
  }

  fn to_char(&self) -> char {
    match self {
      Player.X -> "X",
      Player.O -> "O",
      _ -> " ",
    }
  }
}

struct Board{list(Player)}

impl Board {
  fn default() -> Board {
    {[Player.default() for i in 0..9]}
  }

  fn print_board(&self) {
    //clear();
    let mut out = "|---|---|---|\n";
    for y in 0..3{
      out += "| "
      for x in 0..3{
        print(y * 3 + x)
        let point : Player = self.0[y * 3 + x];
        print(point.to_char())
      }
    }
  }
}

let mut board : Board = Board.default();
board.print_board()

match board.board[0] {
  Player.X{d : data} -> print("Funnyness X"),
  Player.O -> print("Funnyness O"),
  Player.None -> print("Funnyness None"),
}


/*match board.board[0] {
  1 -> print("Ha"), 
  2 -> print("HaHa"), 
  3 -> print("HaHaHa"), 
  4 -> print("HaHaHaHa"), 
  5 -> {
    print("Why pick 5?")
    match 2 {
      1 -> print("TicToc"),
      2 -> print("Whatever"),
    }
    print("HaHaHaHaHa")
  }, 
}*/
