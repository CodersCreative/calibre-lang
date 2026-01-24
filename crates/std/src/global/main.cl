const range = fn(start end : mut int, step : int, inclusive : bool) -> list<int> => {
  if start != INT_MIN && end == INT_MIN => {
    end = start;
    start = 0;
  } else if start == INT_MIN && end == INT_MIN => return [];

  if inclusive => end += step;

  let mut lst : list<int> = [];
  let mut index : int = start;

  for (index < end && step > 0) || (index > end && step < 0) => {
    lst <<= index;
    index += step;
  };

  lst;
};
