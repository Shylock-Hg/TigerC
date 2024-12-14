use std::collections::HashMap;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{braced, Ident, LitStr, Token};

mod kw {
    syn::custom_keyword!(Keywords);
}

struct Keyword {
    name: Ident,
    value: LitStr,
}

impl Parse for Keyword {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        let value = input.parse()?;

        Ok(Keyword { name, value })
    }
}

struct Input {
    keywords: Punctuated<Keyword, Token![,]>,
}

impl Parse for Input {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        input.parse::<kw::Keywords>()?;
        let content;
        braced!(content in input);
        let keywords = Punctuated::parse_terminated(&content)?;

        Ok(Input { keywords })
    }
}

#[derive(Default)]
struct Errors {
    list: Vec<syn::Error>,
}

impl Errors {
    fn error(&mut self, span: Span, message: String) {
        self.list.push(syn::Error::new(span, message));
    }
}

struct Preinterned {
    idx: u32,
    span_of_name: Span,
}

struct Entries {
    map: HashMap<String, Preinterned>,
}

impl Entries {
    fn with_capacity(capacity: usize) -> Self {
        Entries {
            map: HashMap::with_capacity(capacity),
        }
    }

    fn insert(&mut self, span: Span, str: &str, errors: &mut Errors) -> u32 {
        if let Some(prev) = self.map.get(str) {
            errors.error(span, format!("Symbol `{str}` is duplicated"));
            errors.error(
                prev.span_of_name,
                "location of previous definition".to_string(),
            );
            prev.idx
        } else {
            let idx = self.len();
            self.map.insert(
                str.to_string(),
                Preinterned {
                    idx,
                    span_of_name: span,
                },
            );
            idx
        }
    }

    fn len(&self) -> u32 {
        u32::try_from(self.map.len()).expect("way too many symbols")
    }
}

pub(super) fn symbols(input: TokenStream) -> TokenStream {
    let (mut output, errors) = symbols_with_errors(input);

    // If we generated any errors, then report them as compiler_error!() macro calls.
    // This lets the errors point back to the most relevant span. It also allows us
    // to report as many errors as we can during a single run.
    output.extend(errors.into_iter().map(|e| e.to_compile_error()));

    output
}

fn symbols_with_errors(input: TokenStream) -> (TokenStream, Vec<syn::Error>) {
    let mut errors = Errors::default();

    let input: Input = match syn::parse2(input) {
        Ok(input) => input,
        Err(e) => {
            // This allows us to display errors at the proper span, while minimizing
            // unrelated errors caused by bailing out (and not generating code).
            errors.list.push(e);
            Input {
                keywords: Default::default(),
            }
        }
    };

    let mut keyword_stream = quote! {};
    let mut prefill_stream = quote! {};
    let mut entries = Entries::with_capacity(input.keywords.len());

    // Generate the listed keywords.
    for keyword in input.keywords.iter() {
        let name = &keyword.name;
        let value = keyword.value.value();
        let idx = entries.insert(name.span(), &value, &mut errors);
        prefill_stream.extend(quote! {
            #value,
        });
        keyword_stream.extend(quote! {
            pub const #name: Symbol = Symbol(#idx);
        });
    }

    let predefined_size = entries.len();

    let output = quote! {
        pub const PREDEFINED_SIZE: u32 = #predefined_size;

        #[doc(hidden)]
        #[allow(non_upper_case_globals)]
        mod kw_generated {
            use super::Symbol;
            #keyword_stream
        }

        impl IdentPool {
            pub(crate) fn new() -> Self {
                IdentPool::prefill(&[
                    #prefill_stream
                ])
            }
        }
    };

    (output, errors.list)
}
