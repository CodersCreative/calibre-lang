// Enums declaration
enum Language {
  // Enums can have hasmap type data structuress.
	FRENCH { data : int, code : int},
  // Enums can have tuple type data structuress.
	ENGLISH (int),
	SPANISH,
	ARABIC (Language, Language),
}

// Struct declaration
struct CountryBase {
  // Fields in a struct must be explicitly typed.
	language : Language,
}

// Structs can also be declaratied using a tuole structures.
struct Country (Language)

// To add static functions to an object (struct or enum) the impl keyword is used.
impl CountryBase {
  // Methods in the impl block can also be invoked by Object.function()
	fn get_language(self : &CountryBase) -> Language {
		self.language
	}
}
