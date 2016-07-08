module DNA where

transcribeNucleobase 'C' = 'G'
transcribeNucleobase 'G' = 'C'
transcribeNucleobase 'A' = 'U'
transcribeNucleobase 'T' = 'A'

toRNA = map transcribeNucleobase
