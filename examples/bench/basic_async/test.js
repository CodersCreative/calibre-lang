const worker = async (id) => {
  console.log(`worker ${id} done`);
};

const main = async () => {
  const tasks = [];

  for (let i = 0; i < 500000; i++) {
    tasks.push(worker(i));
  }

  await Promise.all(tasks);

  console.log("all workers finished");
};

main();
