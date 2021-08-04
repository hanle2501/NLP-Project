# NLP-Project
A small NLP project which identifies spam emails 

The data is collected from UCI Machine Learning Repository. The link is below
    https://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection

The data is processed as the following:
  1. Get the data
  2. Explore and Visualize Data
  3. Text Processing: expand contractions, clear punctuation, eliminate common words
  4. Train the data using test_train_split model
  5. Create the model using Pipeline which includes CountVectorizer, TfidfTransformer, and RandomForestClassifier
  6. Evaluate the model by creating classification report, confusion matrix
