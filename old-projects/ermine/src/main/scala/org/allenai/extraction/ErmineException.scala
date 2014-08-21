package org.allenai.extraction

/** Exception thrown due to failures in an Ermine processor or pipeline. */
class ErmineException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
