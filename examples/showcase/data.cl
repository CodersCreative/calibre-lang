// Enums declaration
type Language = enum {
	// Enums can have hasmap type data structuress.
	FRENCH	{
		data : int,
		code : int
	},
	// Enums can have tuple type data structuress.
	ENGLISH(int),
	SPANISH,
	ARABIC(Language, Language)
};

// Struct declaration
type CountryBase = struct 	{
	// Fields in a struct must be explicitly typed.
	language : Language
};

// Structs can also be declaratied using a tuole structures.
type Country = struct (Language);

// To add static functions to an object (struct or enum) the impl keyword is used.
impl CountryBase {
	// Methods in the impl block can also be invoked by Object.function()
	const get_language = fn (self : &CountryBase) -> Language => {
		self.language;
	};
};