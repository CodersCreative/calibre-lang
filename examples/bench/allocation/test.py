class Node:
    def __init__(self, node_id, data):
        self.id = node_id
        self.data = data

def main():
    i = 0
    for i in range(1000000):
        temp = Node(
            node_id=i,
            data=[i, i + 1, i + 2]
        )

    print("Finished.")

if __name__ == "__main__":
    main()
