const worker = async (id) => {
  let sum = 0;
  let i = 0;
  
  while (i < 500000) {
    sum += i * i;
    i += 1;
  }

  console.log(`task ${id} = ${sum}`);
};

const main = async () => {
  const tasks = [];

  for (let i = 0; i < 64; i++) {
    tasks.push(worker(i));
  }

  await Promise.all(tasks);

  console.log("all tasks finished");
};

main();
