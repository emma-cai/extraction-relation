package org.allenai.extraction.manager

/** Exception thrown due to failures in extraction. */
class ExtractionException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
