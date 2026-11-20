import asyncio

async def worker(id: int):
    sum = 0
    i = 0

    while i < 500000:
        sum += i * i
        i += 1

    print(f"task {id} = {sum}")

async def main():
    task_count = 64

    async with asyncio.TaskGroup() as tg:
        for i in range(task_count):
            tg.create_task(worker(i))

    print("all tasks finished")

if __name__ == "__main__":
    asyncio.run(main())
