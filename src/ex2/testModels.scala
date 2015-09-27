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
    var num_missing_llh_values = 0
    result.toMap.foreach(pair => pair match { case (ngram, occurrences) => {
      val ngram_llh_result = model.get(ngram)
      ngram_llh_result match {
        case Some(x) => llh += occurrences * x
        case None => num_missing_llh_values += occurrences
      }
    }})

    // Take into account missing values
    val num_total_ngrams = result.size
    llh = llh * num_total_ngrams / (num_total_ngrams - num_missing_llh_values)

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
    var true_positives = MutMap[String, Int]()
    var false_positives = MutMap[String, Int]()
    var failures = Array[String]()

    Source.fromFile(filename).getLines().foreach(line => {
      val split = line.replace("\\s$", "").split(";", 2)
      val lang = split(0).replace("\\s", "")
      val sentence = split(1)

      tries(lang) = tries.getOrElse(lang, 0) + 1

      val detectedLang = detectLanguage(models, sentence)
      if(lang == detectedLang)
        true_positives(lang) = true_positives.getOrElse(lang, 0) + 1
      else {
        false_positives(detectedLang) = false_positives.getOrElse(detectedLang, 0) + 1
        failures = failures :+ "guessed %s, correct %s | %s".format(detectedLang, lang, sentence)
      }
    })

    // Print summary
    print("Test summary:\n\n")

    val total_tp = true_positives.toMap.map(pair => pair match {case (lang, n) => n}).sum
    val total_fp = false_positives.toMap.map(pair => pair match {case (lang, n) => n}).sum
    val total_tries = tries.toMap.map(pair => pair match {case (lang, n) => n}).sum

    print("ALL\t%2d/%2d TP, %2d FP, %5.1f%% accuracy\n\n".format(
      total_tp, total_tries, total_fp, 100*total_tp/total_tries.toFloat))

    tries.keys.foreach(lang => {
      val accuracy = 100 * true_positives.getOrElse(lang, 0) / tries(lang).toFloat
      print("%s\t%2d/%2d TP, %2d FP, %5.1f%% accuracy\n".format(
        lang, true_positives.getOrElse(lang, 0), tries(lang), false_positives.getOrElse(lang, 0), accuracy))
    })

    print("\nFailures:\n")
    failures.foreach(println)
  }

  def main(args: Array[String]): Unit = {
    runTest("data/europarl/test/test_sentences.txt")
  }

}
