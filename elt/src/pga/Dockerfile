# Use an official Python runtime as a parent image
FROM python:3.9-slim-buster

# Copy the current directory contents into the container at /app
COPY . /app

# Set the working directory to /app
WORKDIR /app

# install google chrome
RUN apt-get update && apt-get install -y wget gnupg
RUN wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | apt-key add -
RUN echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list
RUN apt-get update && apt-get install -y google-chrome-stable

# install chromedriver
RUN apt-get install -y curl
RUN apt-get install -yqq unzip
RUN wget -O /tmp/chromedriver.zip https://chromedriver.storage.googleapis.com/$(curl -sS https://chromedriver.storage.googleapis.com/LATEST_RELEASE)/chromedriver_linux64.zip
RUN unzip -j /tmp/chromedriver.zip chromedriver -d /usr/local/bin/
RUN rm /tmp/chromedriver.zip
RUN chmod +x /usr/local/bin/chromedriver

# set display port to avoid crash
ENV DISPLAY=:99

ENV DBPORT 5432
# ENV HOST xxx
# ENV USER xxx
# ENV PASSWORD xxx
# ENV DATABASE xxx

# Install any needed packages specified in requirements.txt
RUN pip3 install --no-cache-dir -r requirements.txt

# Run the command to start the container
CMD ["python3", "main.py"]