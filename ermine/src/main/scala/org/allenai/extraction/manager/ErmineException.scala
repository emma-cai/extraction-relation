package org.allenai.extraction.manager

/** Exception thrown due to failures in an Ermine pipeline. */
class ErmineException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
