import asyncio

async def worker(worker_id: int):
    print(f"worker {worker_id} done")

async def main():
    async with asyncio.TaskGroup() as tg:
        for i in range(100000):
            tg.create_task(worker(i))

    print("all workers finished")

if __name__ == "__main__":
    asyncio.run(main())
