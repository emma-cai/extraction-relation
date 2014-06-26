package org.allenai.extraction.processors.definition

/** A utility object that cleans up a single line of definition to
  * the expected format. The methods provided here can be leveraged
  * by various definition preprocessors for different definition
  * sources (corpora). Each individual corpus-specific preprocessor
  * will do the corpus-specific cleanup/formatting and then use the
  * cleanUp provided here to do more generic formatting/cleanup.
  */
object DefinitionCleanupUtility {
  /** The cleanUp method here can be used to
    * clean up any given text by:
    * - removing parenthesized expressions
    * - removing quoted expressions
    */
  def cleanUp(rawDefinition: String): Seq[String] = {
    // Remove all bracketed expressions
    val parenPattern = """\([^)]*\)""".r
    val defParenStripped = parenPattern replaceAllIn (rawDefinition, "")

    // Remove single quotes from quoted words, like the definition term
    val defSingleQuotedWordPattern = """(\W|^)'([^']+)'(\W|$)""".r
    val defParenSQuotesStripped = defSingleQuotedWordPattern replaceAllIn (defParenStripped, m => m.group(1) + m.group(2) + m.group(3))

    // Remove double quotes 
    val defDoubleQuotedWordPattern = """(\W|^)\"([^\"]+)\"(\W|$)""".r
    val defParenDQuotesStripped = defDoubleQuotedWordPattern replaceAllIn (defParenSQuotesStripped, m => m.group(1) + m.group(2) + m.group(3))

    // Remove empty quoted strings if any
    val defParenEmptyQuotedStringsStripped = defParenDQuotesStripped.replaceAll("''", "").replaceAll("\"\"", "")

    // Break the line up into multiple definitions if separated by semicolons
    defParenEmptyQuotedStringsStripped.split(";").toSeq map { x => x.trim }
  }
}
