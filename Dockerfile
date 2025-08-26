FROM rocker/r-ver:4.3.1

# Install system libraries required for common R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libpq-dev \
    libicu-dev \
    libpng-dev \
    libjpeg-dev \
    libmariadb-dev \
    libsodium-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install Plumber and other required R packages
RUN R -e "install.packages(c('plumber', 'jsonlite','xgboost','stats','dplyr'), repos = 'https://cloud.r-project.org')"

# Debug: print installed packages
RUN R -e "print(installed.packages()[, c('Package', 'Version')])"

# Copy your API files
COPY . /app
WORKDIR /app

# Expose port used by Plumber
EXPOSE 8000

# Start the API
CMD [ "Rscript", "run.R" ]
