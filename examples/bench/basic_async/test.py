import asyncio

async def worker(worker_id: int):
    x = 10000000 / 3 + 5
    print(f"worker {worker_id + x} done")

async def main():
    async with asyncio.TaskGroup() as tg:
        for i in range(800000):
            tg.create_task(worker(i))

    print("all workers finished")

if __name__ == "__main__":
    asyncio.run(main())
