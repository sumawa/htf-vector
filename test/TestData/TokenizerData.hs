module TestData.TokenizerData where

import Data.Text as T
import Data.Set as S
import DataTypes.TfIdfTypes


stopSet = S.fromList ( T.pack <$> ["a","about","above","after","again","against","ain","all","am","an","and","any","are","aren","aren't","as","at","be","because","been","before","being","below","between","both","but","by","can","couldn","couldn't","d","did","didn","didn't","do","does","doesn","doesn't","doing","don","don't","down","during","each","few","for","from","further","had","hadn","hadn't","has","hasn","hasn't","have","haven","haven't","having","he","her","here","hers","herself","him","himself","his","how","i","if","in","into","is","isn","isn't","it","it's","its","itself","just","ll","m","ma","me","mightn","mightn't","more","most","mustn","mustn't","my","myself","needn","needn't","no","nor","not","now","o","of","off","on","once","only","or","other","our","ours","ourselves","out","over","own","re","s","same","shan","shan't","she","she's","should","should've","shouldn","shouldn't","so","some","such","t","than","that","that'll","the","their","theirs","them","themselves","then","there","these","they","this","those","through","to","too","under","until","up","ve","very","was","wasn","wasn't","we","were","weren","weren't","what","when","where","which","while","who","whom","why","will","with","won","won't","wouldn","wouldn't","y","you","you'd","you'll","you're","you've","your","yours","yourself","yourselves"] )

testTxt1 = T.pack "the man went out for a walk"
expectedTok1 = T.pack <$> ["man","went","walk"]

testTxts1 = [("T1","the man went out for a walk"),("T2","the children sat around the fire")]
expectedTokens1 = [("T1",["man","went","out","walk"]),("T2",["children","sat","around","fire"])]

--testStr2 = [("T1","It is going to rain today."),("T2","Today I am not going outside."),("T3","I am going to watch the season premiere.")]
--
--testStr3 = [("T1","The car is driven on the road"),("T2","The truck is driven on the highway.")]
--
--testStr4 = [("T1","one flesh one bone one true religion"),("T2","all flesh is grass"),("T3","one is all all is one")]


