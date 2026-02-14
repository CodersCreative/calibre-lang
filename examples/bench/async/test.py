import threading
import queue

# In Python, we don't have a 'defer' keyword. 
# We usually use try/finally to ensure wg.done() is called.
def worker(worker_id, jobs_queue, results_queue, total_mutex, total_value_list, wg):
    print(f"worker {worker_id} start")
    handled = 0
    try:
        while True:
            try:
                # jobs_queue.get(block=False) acts like Calibre's !jobs.closed() check
                job = jobs_queue.get(timeout=0.1) 
                score = job * (worker_id + 1)
                print(f"worker {worker_id} got {job} => {score}")
                
                results_queue.put(score)
                
                # Python's Mutex (Lock) doesn't wrap the data natively like Calibre's Mutex:<int>
                with total_mutex:
                    total_value_list[0] += score
                
                handled += 1
                jobs_queue.task_done()
            except queue.Empty:
                # If the queue is empty and the main thread has signaled stop
                break
    finally:
        print(f"worker {worker_id} done ({handled} jobs)")
        wg.count_down()

# A simple WaitGroup implementation for Python
class WaitGroup:
    def __init__(self):
        self.count = 0
        self.condition = threading.Condition()

    def increment(self):
        with self.condition:
            self.count += 1

    def count_down(self):
        with self.condition:
            self.count -= 1
            if self.count == 0:
                self.condition.notify_all()

    def wait(self):
        with self.condition:
            while self.count > 0:
                self.condition.wait()

def main():
    jobs_queue = queue.Queue()
    results_queue = queue.Queue()
    total_mutex = threading.Lock()
    total_value = [0] # Use a list to make it mutable inside the lock
    wg = WaitGroup()

    print("start")

    worker_count = 8
    total_jobs = 100 # Reduced for console readability
    
    # Spawn workers
    for i in range(worker_count):
        wg.increment()
        t = threading.Thread(target=worker, args=(i, jobs_queue, results_queue, total_mutex, total_value, wg))
        t.start()
        print(f"spawned worker {i}")

    # Send jobs
    for i in range(1, total_jobs + 1):
        jobs_queue.put(i)

    # In Python, we wait for the queue to be processed
    # results gathering
    received = 0
    while received < total_jobs:
        try:
            value = results_queue.get(timeout=1)
            received += 1
            print(f"received {received} => {value}")
        except queue.Empty:
            break

    wg.wait()
    print(f"all done, total={total_value[0]}")

if __name__ == "__main__":
    main()
