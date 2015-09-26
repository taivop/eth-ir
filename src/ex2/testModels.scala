package ex2

import scala.io.Source
import scala.collection.mutable.{Map => MutMap}

object testModels {

  type Model = Map[String, Double]

  def readModel(lang: String): Model = {
    Source.fromFile(s"data/europarl/models/$lang.txt").getLines()
      .map(line => {
      val split = line.replace("\\s$", "").split("\t") // Split and remove whitespace at end of line
      (split(0), split(1).toDouble)
    }).toMap
  }

  // TODO what to do when key is not found?
  def logLikelihood(model: Model, s: String): Double = {

    // Calculate ngrams
    val result = MutMap[String,Int]()
    for (ngram <- s.toLowerCase().replaceAll("\\s", " ").replaceAll("[^a-zõäöü -]", "").sliding(3)) {
      result(ngram) = result.getOrElse(ngram,0) + 1
    }

    // Calculate log-likelihood
    var llh = 0.0
    result.toMap.foreach(pair => pair match { case (ngram, occurrences) => llh = llh + occurrences * model(ngram)})

    llh
  }

  def detectLanguage(models: Map[String, Model], s: String) = {
    var bestLang = ""
    var bestLLH = Double.NegativeInfinity

    models.foreach(langModelPair => langModelPair match {
      case (lang, model) => {
        val llh = logLikelihood(model, s)
        if(llh > bestLLH) {
          bestLang = lang
          bestLLH = llh
        }
      }
    })

    bestLang
  }

  def runTest(filename: String): Unit = {
    val models = Array("en", "de", "et", "fr", "it").map(lang => (lang, readModel(lang))).toMap


    // Run through all test cases
    var tries = MutMap[String, Int]()
    var successes = MutMap[String, Int]()

    Source.fromFile(filename).getLines().foreach(line => {
      val split = line.replace("\\s$", "").split(";", 2)
      val lang = split(0).replace("\\s", "")
      val sentence = split(1)

      tries(lang) = tries.getOrElse(lang, 0) + 1

      if(lang == detectLanguage(models, sentence))
        successes(lang) = successes.getOrElse(lang, 0) + 1
    })

    // Print summary
    println("Test summary:")
    tries.keys.foreach(lang => {
      val accuracy = 100 * successes.getOrElse(lang, 0) / tries(lang).toFloat
      print("%s\t%d/%d correct, %.1f%% accuracy\n".format(lang, successes.getOrElse(lang, 0), tries(lang), accuracy))
    })
  }

  def main(args: Array[String]): Unit = {
    runTest("data/europarl/test/test_sentences.txt")
  }

}
