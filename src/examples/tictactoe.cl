type Player = enum {
  None,
  X,
  O
}

impl Player {
  const default = fn() -> Player => return Player.None;

  const get_opposite = fn(self : &Player) -> Player => {
    match self {
      Player.X => Player.O,
      Player.O => Player.X,
      _ => Player.None,
    }
  }

  const switch = fn(self : &mut Player) => self = self.get_opposite(self);

  const to_char = fn(self : &Player) -> char => {
    match self {
      Player.X => "X",
      Player.O => "O",
      _ => " ",
    }
  }
}

const get_pos = fn(x : int, y : int) -> int => y * 3 + x;

const get_user_input = fn() -> int => {
  let mut x : int = std.console.input("Enter x position (min = 1, max = 3): ");
  let mut y : int = std.console.input("Enter y position (min = 1, max = 3): ");
  x--; y--;
  get_pos(x, y);
}

type Board = struct(list<Player>)

impl Board {
  const default = fn() -> Board => Board([Player.None for i in 0..9])

  const print_board = fn(self : &Board) => {
    // std.console.clear();
    print("|---|---|---|");

    for y in 0..3 => {
      let mut out : string = "| ";
      for x in 0..3 => {
        let point : Player = self.0[y * 3 + x];
        out += point.to_char();
        out += " | "; 
      }
      print(out);
      print("|---|---|---|");
    }
  }

  const is_full = fn(self : &Board) -> bool => {
    for i in 0..9 => if self.0[i] == Player.None => return false;
    return true;
  } 

  const get_winner = fn(self : &Board) -> Player => {
    for y in 3 => {
      let y_adj = y * 3;
      if self.0[y_adj] != Player.None && self.0[y_adj] == self.0[y_adj+1] && self.0[y_adj] == self.0[y_adj+2] => {
        return self.0[y_adj];
      }
    }
    
    for x in 3 => if self.0[x] != Player.None && self.0[x] == self.0[x+3] && self.0[x] == self.0[x+6] => return self.0[x];

    if self.0[0] != Player.None && self.0[0] == self.0[4] && self.0[0] == self.0[8] => return self.0[0];

    if self.0[2] != Player.None && self.0[2] == self.0[4] && self.0[2] == self.0[6] => return self.0[2];

    Player.None
  }

  const get_pos_value = fn(self : &Board, x : int, y : int) -> Player => self.0[get_pos(x, y)];
  
  const get_user_input_checked = fn(self : &Board) -> int => {
    let mut index = -1;

    for index < 0 || index > 9 || self.0[index] != Player.None => {
      // std.console.clear();
      self.print_board();
      if index != -1 => print("Try again.");
      index = get_user_input();
    }

    print(index);
    index
  }

  const set_user_input = fn(self : &mut Board, player : Player) => self.0[self.get_user_input_checked()] = player;

  const get_best_position = fn(self : &mut Board, player : Player) -> int => {
    let maximising = player == Player.X
    let mut best_pos = 0;
    let mut best_value = if maximising => INT_MAX else => INT_MIN
    
    for i in 9 => {
      if self.0[i] != Player.None => continue;
      
      self.0[i] = player;
      let value = self.minimax(player.get_opposite(), 0);
      self.0[i] = Player.None;
      
      if maximising => if best_value < value => {
          best_value = value;
          best_pos = i;
        }
      else => if best_value > value => {
          best_value = value;
          best_pos = i;
        }
    }
    
    best_pos
  }

  const minimax = fn(self : &mut Board, player : Player, depth : int = 0) -> int => {
    match board.get_winner() {
      Player.X => return 100 - depth,
      Player.Y => return -100 + depth,
      Player.None if board.is_full() => return 0,
    }

    let maximising = player == Player.X 

    let mut best_value = if maximising => INT_MAX else => INT_MIN
    
    for i in 9 => {
      if self.0[i] != Player.None => continue;

      self.0[i] = player;
      let value = self.minimax(player.get_opposite(), depth + 1);
      self.0[i] = Player.None;

      if maximising => if best_value < value => best_value = value;
      else => if best_value > value => best_value = value;
    }
    best_value;
  }
}

const get_ais = fn() -> <bool, bool> => {
  let mut ais = (false, false);
  
  if trim(std.console.input("Enter 'y' to make Player X (1) an AI: ")) == "y" => ais.0 = true;
  if trim(std.console.input("Enter 'y' to make Player O (2) an AI: ")) == "y" => ais.1 = true;

  ais
}

const main = fn() => {
  for true => {
    let ais = get_ais();
    print(ais);
    let both_ais = ais.0 && ais.1;
    let mut board : Board = Board.default();
    let mut player : Player = Player.X;

    for !board.is_full() && board.get_winner() == Player.None => {
      // std.console.clear();

      board.print_board();

      if ais.0 && Player.X == player || ais.1 && Player.O == player => {
        board.0[board.get_best_position(player)] = player; 
        if both => std.thread.wait(4);
        player.switch();
        continue;
      }

      board.set_user_input(player);
      player.switch();
    }
    // std.console.clear();

    board.print_board();

    match board.get_winner() {
      Player.X => print("Player X Won!"),
      Player.O => print("Player O Won!"),
      Player.None => print("Tied!"),
    }
  }
}

main();
