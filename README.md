# skipgram
skip-gram extended n-gram tokenizer for NLP

This tokenizer will produce regular n-grams and skip-grams [token X token] where X can be specified {1L..N} to skip as many tokens.
The input character list may be compressed (specify cflag = 1L) and additional filtering can be performed simultaneously when a filter list
is specified. Please refer to description and examples for input parameters.
The output is a named integer.
