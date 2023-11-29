# Arrow Memes
Just displays some text with an arrow, but the text and orientation depend on the values in config.txt at compile time, rather than runtime, so you can send the compiled program and it will include the text you specify. If you're wondering why on Earth anyone would want such a thing, it's an in-joke.

# Preparing Memes
The format of config.txt is "key=value" pairs on separate lines. The key "text" controls the text, "direction" can be one of "up", "down", "left", or "right" and controls the arrow's direction, and "reverse" makes the arrow point towards the text if its value is not empty or "false".

Compilation requires the Haskell Stack tool, and the command is `stack build`. If you change config.txt and compile again, it may not notice that it's meant to update it.
