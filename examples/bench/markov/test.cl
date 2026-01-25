// calibre-lang: Echo Markov Generator (Revised for Current Implementation)

type Frequency = struct {
    word: str,
    count: int // Using 'mut' field for in-place updates
};

type ChainLink = struct {
    prefix: str,
    successors: list:<Frequency>
};

impl ChainLink {
    // Returns a mutable reference to the frequency struct if found
    const find_successor = fn(self: &mut ChainLink, word: str) -> &mut Frequency? => {
        // Using 'for f' to iterate through the list
        for f in self.successors => {
            if f.word == word => {
                return some(f);
            };
        };
        none();
    };
};

// --- CORE LOGIC ---

const tokenize = fn(input: str) -> list:<str> => {
    // TODO: Implement string.split() in the Calibre stdlib
    return list:<str>["the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"];
};

// Search helper for the chain
const find_link = fn(c: &mut list:<ChainLink>, target: str) -> &mut ChainLink? => {
    for link in c => {
        if link.prefix == target => {
            return some(link);
        };
    };
    none();
};

const build_chain = fn(tokens: list:<str>) -> list:<ChainLink> => {
    let mut chain = list:<ChainLink>[];

    // Using a range to iterate and build bigrams
    for i in 0..(tokens.len() - 1) => {
        let current = tokens[i];
        let next = tokens[i + 1];

        // Using match + try for the current logic flow
        match find_link(chain, current) {
            .Some : link => {
                // Interior mutability via &mut
                match link.find_successor(next) {
                    .Some : freq => {
                        freq.count = freq.count + 1;
                    },
                    .None => {
                        link.successors <<= Frequency{ word: next, count: 1 };
                    }
                };
            },
            .None => {
                chain <<= ChainLink{
                    prefix: current,
                    successors: list[Frequency{ word: next, count: 1 }]
                };
            }
        };
    };
    chain;
};

// --- GENERATION ---

const generate_text = fn(chain: &mut list:<ChainLink>, start_word: str, length: int) -> str => {
    let mut output = start_word;
    let mut current = start_word;

    for _ in 0..length => {
        // Using Tier 2 'try' syntax for option fallback
        let next_link = try find_link(chain, current) => break;

        // TODO: Implement a real random selector. For now, we take the first successor.
        let next_word = next_link.successors[0].word;

        output = output & " " & next_word;
        current = next_word;
    };

    output;
};

// --- MAIN ---

const main = fn() => {
    let raw_input : str = "the quick brown fox jumps over the lazy dog";

    let mut chain = raw_input
    |> tokenize($)
    |> build_chain($);

    // Generation phase
    let result = generate_text(chain, "the", 5);

    print(result);
};
