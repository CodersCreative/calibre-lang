const worker = async (id) => {
  const x = 10000000 / 3 + 5;
  console.log(`worker ${id + x} done`);
};

const main = async () => {
  const tasks = [];

  for (let i = 0; i < 800000; i++) {
    tasks.push(worker(i));
  }

  await Promise.all(tasks);

  console.log("all workers finished");
};

main();
