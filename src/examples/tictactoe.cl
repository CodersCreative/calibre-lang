enum Player{
  None,
  X,
  O
}

impl Player {
  fn default() -> Player {
    return Player.None;
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
        //print("Yass")
        print(self.0(y * 3 + x))
      }
    }
  }
}

let mut board : Board = Board.default();
board.print_board()

