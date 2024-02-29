library(tidyverse)

original_message = 'I am working on my statistics homework,
and I am finding the encryption example to be especially fascinating.'

# Function to apply Caesar cipher shift
shift_cipher = function(text, shift) {
  alphabet = paste0(LETTERS, collapse = "")
  
  # Remove non-letters and convert to uppercase
  clean_text = gsub("[^A-Za-z] ", "", text)
  clean_text = toupper(clean_text)
  
  shifted_alphabet = paste0(substring(alphabet, shift + 1, 26), substring(alphabet, 1, shift))
  shifted_text = chartr(alphabet, 
                        shifted_alphabet, 
                        clean_text)
  return(shifted_text)
}

# example message
my_shift = 6
encoded_message = shift_cipher(toupper(original_message), my_shift)
encoded_message

# this function shifts backwards
invert_shift_cipher = function(text, shift) {
  alphabet = paste0(LETTERS, collapse = "")
  shifted_alphabet = paste0(substring(alphabet, 27 - shift, 26), substring(alphabet, 1, 26 - shift))
  
  # Remove non-letters and convert to uppercase
  clean_text = gsub("[^A-Za-z] ", "", text)
  clean_text = toupper(clean_text)
  
  shifted_text = chartr(alphabet, 
                        shifted_alphabet, 
                        clean_text)
  return(shifted_text)
}

# apply to the encoded message
invert_shift_cipher(encoded_message, my_shift)



# Let's now try this message:
encoded_message = "WT MCI KOBH USH TFCA XSGHSF HC KSZQV, 
MCI BSSR HC KOZY BCFHV CB GDSSRKOM, 
WRSOZZM RCRUWBU HVS AOBWOQG CB FSBHOZ 
GQCCHSFG QOFSSFWBU HVFCIUV HVS QFCKR 
OH TWTHSSB AWZSG DSF VCIF."

# and what about this message?
encoded_message2 = "RW SDBC J ONF FNNTBCQN AXJMFJHB
JWM YJATB JAXDWM JDBCRW FRUU KN PUXARXDBUH ODUU
XO KUDNKXWWNCBOXA FQRLQ FN LJW CQJWT UJMH KRAM
SXQWBXW JWM QNA QRPQFJH KNJDCRORLJCRXW YAXSNLCB
XO CQN BRGCRNB JWM BNENWCRNB"

invert_shift_cipher(encoded_message, 1)


### Idea:
# Let's compare the letter frequency distribution
# to the distribution of letters in "typical" English sentences
# If a hypothesized shift results in a message that more closely matches
# the letter distribution in English, it's a better candidate decoding.

# start with gutenberg.r

# Import letter frequencies
letter_frequencies = read.csv("../data/letter_frequencies.csv")

# This will calculate the chi-squared goodness of fit statistic
# for an input sentence, based on a frequency table of letters
calculate_chi_squared = function(sentence, freq_table) {
  
  # Ensure letter frequencies are normalized and sum to 1
  freq_table$Probability = freq_table$Probability / sum(freq_table$Probability)
  
  # Remove non-letters and convert to uppercase
  clean_sentence = gsub("[^A-Za-z]", "", sentence)
  clean_sentence = toupper(clean_sentence)
  
  # Count the occurrences of each letter in the sentence
  observed_counts = table(factor(strsplit(clean_sentence, "")[[1]], levels = freq_table$Letter))
  
  # Calculate expected counts
  total_letters = sum(observed_counts)
  expected_counts = total_letters * freq_table$Probability
  
  # Chi-squared statistic
  chi_squared_stat = sum((observed_counts - expected_counts)^2 / expected_counts)
  
  return(chi_squared_stat)
}

# Let's try a guess
decoded_message = invert_shift_cipher(encoded_message, 7)
calculate_chi_squared(decoded_message, letter_frequencies)

decoded_message = invert_shift_cipher(encoded_message, 14)
calculate_chi_squared(decoded_message, letter_frequencies)

# Let's try all shifts
chi_sq = rep(0, 25)
for(shift in 1:25) {
  decoded_message = invert_shift_cipher(encoded_message, shift)
  chi_sq[shift] = calculate_chi_squared(decoded_message, letter_frequencies)
}

plot(chi_sq, log='y')

# Let's use that shift:
invert_shift_cipher(encoded_message, 14)


# Now the second message:
chi_sq = rep(0, 25)
for(shift in 1:25) {
  decoded_message = invert_shift_cipher(encoded_message2, shift)
  chi_sq[shift] = calculate_chi_squared(decoded_message, letter_frequencies)
}

plot(chi_sq, log='y')

# Let's use that shift:
invert_shift_cipher(encoded_message2, 9)
