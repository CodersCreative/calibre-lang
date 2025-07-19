enum Language {
  // Enums can have hasmap type data structuress.
	FRENCH { data : int, code : int},
  // Enums can have tuple type data structuress.
	ENGLISH (int),
	SPANISH,
	ARABIC (Language),
}

let mut recursive_language = Language.ARABIC(Language.FRENCH{data : 5, code : 10})

match &mut recursive_language {
  Language.ARABIC(variant) -> {
    print(variant);
    match &mut variant {
      Language.FRENCH{data} -> {
        print("Hello");
        print("Enum: " + data)
        data = 5;
        print("Enum Changed: " + data)
      }
    }
  },
}
