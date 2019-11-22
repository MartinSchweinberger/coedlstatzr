#' Coding of 5,000 Speech-Units from of spoken colloquial New Zealand English
#'
#' A dataset containing the coding of 5,000 speech-units of New Zealand English for instances of speech-unit final EH, a speaker id as well as the age, gender, and ethnicity of the speaker.
#'
#' @format A data frame with 5,000 rows and 5 variables:
#' \describe{
#'   \item{ID}{speaker identification}
#'   \item{Gender}{gender of speaker (Men versus Women)}
#'   \item{Age}{age of speaker (Young versus Old)}
#'   \item{Ethnicity}{ethnicity of speaker (Maori versus Pakeha)}
#'   ...
#' }
"blrdata"

#' Coding of 250 cases of a fictional data set
#'
#' A dataset containing the 250 cases representing speakers with certain sociodemographic features and their use of an unnamed linguistic feature.
#'
#' @format A data frame with 5,000 rows and 5 variables:
#' \describe{
#'   \item{Variety}{vareity of English (American versus British)}
#'   \item{Gender}{gender of speaker (Men versus Women)}
#'   \item{Age}{age of speaker (Young versus Old)}
#'   \item{Class}{socioeconomic class of speaker (Middle versus Working)}
#'   \item{Frequency}{freqeuncy counts of an unnamed linguistic feature}
#'   ...
#' }
"cfadata"

#' Fictional Data set of 303 Users and Non-Users of Discourse Like.
#'
#' A fictional dataset containing the coding of 303 speakers that are either users of discourse like or not and their social chracteristics  EH, a speaker id as well as the age, gender, and ethnicity of the speaker.
#'
#' @format A data frame with 303 rows and 4 variables:
#' \describe{
#'   \item{Gender}{Gender of speaker (Man versus Woman)}
#'   \item{Age}{Age of speaker (Young versus Old)}
#'   \item{SocialStatus}{Social status of speakers (High versus Low)}
#'   \item{LikeUser}{Speaker is a user of discourse like (1: yes versus 0:no)}
#'   ...
#' }
"citdata"

#' Fictional Data of 500 Conversations Coded for Number of Fillers (uhm).
#'
#' A fictional data set representing 500 conversations coded for the number of pauses filled with "uhm" along with information about the conversation and speakers.
#'
#' @format A data frame with 303 rows and 6 variables:
#' \describe{
#'   \item{ID}{Identification of the conversation.}
#'   \item{Trial}{Integer showing whether the conversation was recorded on day 1, 2, or 3.}
#'   \item{Language}{Factor representing the language in which the conversation took place (English, German, Russian, Mandarin).}
#'   \item{Gender}{Factor representing the gender of speakers (Man versus Woman).}
#'   \item{UHM}{Frequency of pauses filled with uhm.}
#'   \item{Shots}{Number of shots a speaker drank before the converstion.}
#'   ...
#' }
"countdata"

#' 537 Texts coded for their Relative Number of Prepositions.
#'
#' Data set representing 537 texts coded for the relative frequency of prepositions in them along with information about the texts and their writers.
#'
#' @format A data frame with 537 rows and 5 variables:
#' \describe{
#'   \item{Date}{Year of composition.}
#'   \item{Genre}{Genre of the text.}
#'   \item{Text}{Author of the text.}
#'   \item{Gender}{Factor representing the gender of speakers (Man versus Woman).}
#'   \item{Prepositions}{Frequency of prepositions per 1,000 words.}
#'   \item{Region}{Region in which the text was written.}
#'   ...
#' }
"lmmdata"

#' Fictional Data Set Representing 2000 Speech-Units Coded for Speech-Unit Final Discourse Like.
#'
#' Fictional data set representing 2000 speech-units coded for whether they ended with speech-unit final like along with information about the sentences and their speakers.
#'
#' @format A data frame with 2000 rows and 6 variables:
#' \describe{
#'   \item{ID}{Anonymous identification sequence for the speaker.}
#'   \item{Gender}{Factor representing the gender of speakers (Men versus Women).}
#'   \item{Age}{Factor representing the age of speakers (Old versus Young).}
#'   \item{ConversationType}{Factor representing whether the conversation took place between interlocutors of the same gender or not (MixedGender versus SameGender).}
#'   \item{Priming}{Factor representing if another instance of speech-unit final like had occured shortly before (NoPrime versus Prime).}
#'   \item{SUFLike}{Interger representing whether the speech-unit ended with discourse like or not.}
#'   ...
#' }
"mblrdata"

#' Data Set Representing the Price of 100 Presents.
#'
#' Fictional data set taken from Field, Andy, Miles, Jeremy, and Field, Zoe. 2012. Discovering Statistics Using R. SAGE which represents the prices of 100 presents along with information about the buyer of the present.
#'
#' @format A data frame with 100 rows and 3 variables:
#' \describe{
#'   \item{status}{Factor representing the relationship status of the buyer (Relationship versus Single).}
#'   \item{attraction}{Factor representing if the buyer is attracted to the Receiver of the Present (Intersted versus NotIntersted).}
#'   \item{money}{Numeric variable encoding the price of the present.}
#'   ...
#' }
"mlrdata"

#' Fictional Data Set Representing 6 Speakers.
#'
#' Fictional data representing 6 speakers and providing information about their age and whether they are users of discourse like or not.
#'
#' @format A data frame with 6 rows and 2 variables:
#' \describe{
#'   \item{Age}{Integer representing the age of speakers.}
#'   \item{LikeUser}{Factor representing if the speaker is a user of discourse like or not (no versus yes).}
#'   ...
#' }
"mlrdata"

#' Fictional Data Set Representing 400 observations of Likert Responses.
#'
#' Fictional data taken from the UCLA Institute for Digital Research and Education (https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/) representing 400 responses to Likert items and information about the background of the responders.
#'
#' @format A data frame with 400 rows and 3 variables:
#' \describe{
#'   \item{pared}{Factor representing whether at least one of the student's parents has a graduate degree (1: yes versus 0: no).}
#'   \item{public}{Factor representing whether the student attended a public or private high-school (1: public versus 0: private).}
#'   \item{gpa}{Numeric variable representing the student’s grade point average.}
#'   ...
#' }
"ordinaldata"

#' Fictional Data Set Representing 200 observations of Pauses in Conversations.
#'
#' Fictional data representing 200 conversations and the number of pauses in the conversations along with information about the conversation.
#'
#' @format A data frame with 200 rows and 4 variables:
#' \describe{
#'   \item{Id}{Identification code of the conversation.}
#'   \item{Pauses}{Number of pauses in a conversation.}
#'   \item{Language}{Factor representing the language of the conversation (English versus Russian versus German).}
#'   \item{Alcohol}{Numeric variable representing mililiters of alcohol drunk by the speaker.}
#'   ...
#' }
"posdata"

#' Fictional Data Set Representing 500 observations of Pauses in Conversations.
#'
#' Fictional data representing 500 conversations and the number of pauses in the conversations along with information about the conversation.
#'
#' @format A data frame with 500 rows and 8 variables:
#' \describe{
#'   \item{ID}{Identification code of the conversation.}
#'   \item{Trial}{Integer showing whether the conversation was recorded on day 1, 2, or 3.}
#'   \item{Language}{Factor representing the language in which the conversation took place (English, German, Russian, Mandarin).}
#'   \item{Gender}{Factor representing the gender of speakers (Man versus Woman).}
#'   \item{UHM}{Frequency of pauses filled with uhm.}
#'   \item{Noise}{Factor representing if music was playing in the background (with versus Without).}
#'   \item{Interlocutor}{Factor representing the gender of the interlocutor (OtherGender versus SameGender).}
#'   \item{Shots}{Number of shots a speaker drank before the converstion.}
#'   ...
#' }
"posdata2"

#' Fictional Data Set Representing 500 observations of Pauses in Conversations.
#'
#' Fictional data representing 60 test scores of two groups of students.
#'
#' @format A data frame with 60 rows and 2 variables:
#' \describe{
#'   \item{Group}{Factor representing the group in which the score was achieved.}
#'   \item{Score}{Numerc value representing the test score.}
#'   ...
#' }
"slrdata2"


