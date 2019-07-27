import pandas as pd
import numpy as np
import emoji #for emojis processing 
import regex

def parse_chat_data(chat_data_location, processed_data_location):
    data = pd.read_csv(chat_data_location, sep = "\n", header = None)
    data.columns = ["message"]
    
    def extract_emojis_list(text):
    # This function takes in text and returns a list of emojis. 
    # Need emoji library for this
    # Borrowed from here: https://stackoverflow.com/questions/43146528/how-to-extract-all-the-emojis-from-text/50530149#50530149 
        emoji_list = []
        data = regex.findall(r'\X', text)
        for word in data:
            if any(char in emoji.UNICODE_EMOJI for char in word):
                emoji_list.append(word)

        return emoji_list

    def sessionize_udf(df, dtdiff):
        # Helper function to sessionize data. 
        # takes a dataframe and date differences in minutes 
        # Function can be used to make sessions that are not spearated by a period longer than X minutes.
        # Returns a list that can be assigned to a new column
        df = df.sort_values(by = "id") #sort 
        df["event_dt_lagged"] = df["event_dt"].shift(1) #get lagged time 
        df["diff"] = (df.event_dt - df.event_dt_lagged).astype("timedelta64[m]") #calculate minute difference 
        df["session_id"] = df["diff"].apply(lambda x: 1 if (x > dtdiff) | (np.isnan(x)) else 0) # make a flag for new session start
        return df.session_id.cumsum() #get session id 
    
    data_e = data.message.str.extract(r"(^\d*\/\d*\/\d{2}, \d*:\d*)?(?: - )?(.+?: )?(.*)") #get 3 groupos: event_dt, author, text
    data_e.columns = ["event_dt", "author", "text"] #rename columns 
    data_e.author = data_e.author.str.replace(":", "") #get rid of : in author name
    data_e = data_e.fillna(method = 'ffill') #fill missing values forward
    data_e = data_e.dropna() #drop na - this will get rid of first message without author from WhatsApp
    data_e["text"] = data_e.groupby(["event_dt", "author"])["text"].transform(' '.join) #merge rows in the same minute
    data_e = data_e.drop_duplicates(["event_dt", "author","text"]) #drop dups
    data_e["event_dt"] = pd.to_datetime(data_e.event_dt, format = "%m/%d/%y, %H:%M" ) #convert to date
    data_e["media_flag"] = data_e.text.str.contains("<Media omitted>")
    data_e["link_flag"] = data_e.text.str.contains("http://|https://")
    data_e["id"] = np.arange(len(data_e)) #make row id
    data_e["emojis"] = data_e.text.apply(extract_emojis_list)
    data_e["emoji_description"] = data_e.emojis.apply(lambda emojis_list: [emoji.demojize(x) for x in emojis_list])
    data_e["date"] = data_e.event_dt.dt.date
    data_e["hour"] = data_e.event_dt.dt.hour
    data_e["weekday"] = data_e.event_dt.dt.weekday_name
    data_e["session_id60"] = sessionize_udf(data_e, 60) #sessions without a break more than 1 hour
    data_e["session_id180"] = sessionize_udf(data_e, 60*3) #session without a break more than 3 hours
    data_e["message_lag"] = (data_e.event_dt - data_e.event_dt.shift(1)).astype("timedelta64[m]")
    
    data_e.to_csv(processed_data_location, index = False)