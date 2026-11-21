package main

import (
	"fmt"
	"sync"
)

func worker(id int, wg *sync.WaitGroup) {
	defer wg.Done()

	sum := 0
	for i := 0; i < 500000; i++ {
		sum += i * i
	}

	fmt.Printf("task %d = %d\n", id, sum)
}

func main() {
	var wg sync.WaitGroup

	for i := 0; i < 64; i++ {
		wg.Add(1)
		go worker(i, &wg)
	}

	wg.Wait()
	fmt.Println("all tasks finished")
}
