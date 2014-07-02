package org.allenai.extraction.processors.definition

import com.escalatesoft.subcut.inject.NewBindingModule

/** Module providing an instance of OtterNounDefinitionExtractor.
  */
class OtterNounDefinitionExtractorModule(dataDirectory: String) extends NewBindingModule(implicit module => {
  import module._
  import scala.concurrent.ExecutionContext.Implicits.global
  module.bind[OtterNounDefinitionExtractor] toSingle new OtterNounDefinitionExtractor(dataDirectory)
})