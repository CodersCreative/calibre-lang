import asyncio
from pathlib import Path

def sum_interval(low: int, high: int) -> int:
    if high < low:
        return 0
    return ((low + high) * (high - low + 1)) // 2

def handle_range(cur: str) -> int:
    start_s, end_s = cur.split("-", 1)
    start = int(start_s)
    end = int(end_s)
    if end < start:
        return 0

    total = 0
    for k in range(1, 11):
        p = 10**k
        mul = p + 1
        min_x = 1 if k == 1 else (p // 10)
        max_x = p - 1
        low = (start + mul - 1) // mul
        high = end // mul
        x_low = max(low, min_x)
        x_high = min(high, max_x)
        if x_low <= x_high:
            total += mul * sum_interval(x_low, x_high)
    
    return total

async def main() -> None:
    input_path = Path(__file__).with_name("input.txt")
    text = input_path.read_text()
    ranges = [r.strip() for r in text.split(",") if r.strip() != ""]

    worker_count = 4

    def worker_sum(ranges: list[str], start_idx: int, step: int) -> int:
        return sum([handle_range(ranges[idx]) for idx in range(start_idx, len(ranges), step)])

    tasks = [
        asyncio.to_thread(worker_sum, ranges, i, worker_count)
        for i in range(worker_count)
    ]

    parts = await asyncio.gather(*tasks)
    print(sum(parts))

if __name__ == "__main__":
    asyncio.run(main())
