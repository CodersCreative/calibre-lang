enum Player{
  None,
  X,
  O
}

impl Player {
  fn default() -> Player {
    return Player.None;
  }

  fn get_opposite(&self) -> Player {
    match self {
      Player.X -> Player.O,
      Player.O -> Player.X,
      _ -> Player.None,
    }
  }

  fn switch(&mut self) {
    self = self.get_opposite(self);
  }

  fn to_char(&self) -> char {
    match self {
      Player.X -> "X",
      Player.O -> "O",
      _ -> " ",
    }
  }
}

fn get_pos(x : int, y : int) -> int {
  y * 3 + x
}

fn get_user_input() int {
  let x : int = input("Enter x position (min = 1, max = 3): ");
  let y : int = input("Enter y position (min = 1, max = 3): ");
  x--; y--;
  get_pos(x, y);
}

struct Board{list(Player)}

impl Board {
  fn default() -> Board {
    {[Player.default() for i in 0..9]}
  }

  fn print_board(&self) {
    clear();
    print("|---|---|---|");

    for y in 0..3{
      let mut out = "| ";
      for x in 0..3{
        print(y * 3 + x)
        let point : Player = self.0[y * 3 + x];
        out += point.to_char();
        out += " | "; 
      }
      print(out);
      print("|---|---|---|");
    }
  }

  fn is_full(&self) -> bool {
    for i in 9 {
      if self.0[i] == Player.None {
        return false;
      } 
    }
    true
  }

  fn get_winner(&self) -> Player {
    for y in 3 {
      let y_adj = y * 3;
      if self.0[y_adj] != Player.None && self.0[y_adj] == self.0[y_adj+1] && self.0[y_adj] == self.0[y_adj+2] {
        return self.0[y_adj];
      }
    }
    
    for x in 3 {
      if self.0[x] != Player.None && self.0[x] == self.0[x+3] && self.0[x] == self.0[x+6] {
        return self.0[x];
      }
    }

    if self.0[0] != Player.None && self.0[0] == self.0[4] && self.0[0] == self.0[8] {
      return self.0[0];
    }

    if self.0[2] != Player.None && self.0[2] == self.0[4] && self.0[2] == self.0[6] {
      return self.0[2];
    }

    Player.None
  }

  fn get_pos_value(&self, x : int, y : int) -> Player {
    self.0[get_pos(x, y)];
  }
  
  fn get_user_input(&self) -> Player {
    let mut index = -1;

    for index < 0 || index > 9 || self.board[index] != Player.None {
      clear();
      self.print_board();
      if index != -1 {
        print("Try again.");
      }
      index = get_user_input();
    }

    index
  }

  fn set_user_input(&mut self, player : Player) {
    self.0[self.get_user_input()] = player;
  }

  fn get_best_position(&mut self, player : Player) int {
    let maximising = if player == Player.X {true} else {false}
    let mut best_pos = 0;
    let mut best_value = if maximising {INT_MAX} else {INT_MIN}
    
    for i in 9 {
      if self.0[i] != Player.None{
        continue;
      }
      
      self.0[i] = player;
      let value = self.minimax(player.get_opposite(), 0);
      self.0[i] = Player.None;
      
      if maximising {
        if best_value < value {
          best_value = value;
          best_pos = i;
        }
      }else{
        if best_value > value {
          best_value = value;
          best_pos = i;
        }
      }
    }
    
    best_pos
  }

  fn minimax(&mut self, player : Player, depth : int = 0) int {
    match board.get_winner() {
      Player.X -> return 100 - depth,
      Player.Y -> return -100 + depth,
      Player.None if board.is_full() -> return 0,
    }

    let maximising = if player == Player.X {true} else {false}

    let mut best_value = if maximising {INT_MAX} else {INT_MIN}
    
    for i in 9 {
      if self.0[i] != Player.None{
        continue;
      }

      self.0[i] = player;
      let value = self.minimax(player.get_opposite(), depth + 1);
      self.0[i] = Player.None;
      if maximising {
        if best_value < value {
          best_value = value;
        }
      }else{
        if best_value > value {
          best_value = value;
        }
      }
    }
    best_value;
  }
}

fn get_ais() -> list(int) {
  let mut ais = [];

  if trim(input("Enter 'y' to make Player X (1) an AI: ")) == "y" {
    ais[0] = Player.X;
  }else {
    ais[0] = Player.None
  }
  
  if trim(input("Enter 'y' to make Player O (2) an AI: ")) == "y" {
    ais[1] = Player.O;
  }else {
    ais[1] = Player.None
  }

  ais
}

fn main() {
  for true {
    let ais = get_ais();
    let both_ais = ais[0] != Player.None && ais[1] != Player.None;
    let mut board : Board = Board.default();
    let mut player : Player = Player.X;

    for !board.is_full() && board.get_winner() == Player.None {
      clear;
      board.print_board();
      if ais[0] == player || ais[1] == player {
        board.0[board.get_best_position(player)] = player; 
        if both {
          wait(4);
        }
        player.switch();
        continue;
      }

      self.set_user_input(player);
      player.switch();
    }
    clear;
    board.print_board();

    match board.get_winner() {
      Player.X -> print("Player X Won!"),
      Player.O -> print("Player O Won!"),
      Player.None -> print("Tied!"),
    }
  }
}


