# Copyright 2021-2023 Louis Héraut (louis.heraut@inrae.fr)*1,
#                     Éric Sauquet (eric.sauquet@inrae.fr)*1
#
# *1   INRAE, France
#
# This file is part of ASHE R package.
#
# ASHE R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# ASHE R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ASHE R package.
# If not, see <https://www.gnu.org/licenses/>.



### 2.3. FDC _______________________________________________________
#' @title fdc_values
#' @description Given a vector of streamflow values, computes a
#' data.frame with two columns : a 'p' column containing the
#' probability of exceedance and a 'Q' column containing the
#' corresponding streamflow values. Two methods can be used : simply
#' sorting the data (not recommended) or using the quantile function.
#' @param Q Streamflow vector
#' @param n number of rows in the resulting data.frame (should be
#' smaller than the length of 'Q'.
#' @param sort logical. Should the sort function be used instead
#' of the quantile function ?
#' @param na.rm logical. Should the missing values be ignored ? (must
#' be TRUE if the quantile function is used !)
#' @return
#' @export
fdc_values = function (Q, n=1000, sort=FALSE, na.rm=TRUE) {
    if (na.rm) {
        Q = Q[!is.na(Q)]
    }
    if (sort) {
        m = length(Q)
        pfdc = 1-1:m/m
        Qfdc = sort(Q, na.last=ifelse(na.rm, NA, FALSE))
    } else {
        if (n > length(Q)) {
            warning("'n' is larger than the number of values in 'Q'!")
        }
        pfdc = seq(0, 1, length.out=n)
        Qfdc = compute_Qp(Q, p=pfdc)
    }
    return(dplyr::tibble(p=pfdc, Q=Qfdc))
}


## 1. HYDROGRAPH _____________________________________________________
# Computes the hydrograph of a station
#' @title Hydrograph
#' @export
get_hydrograph = function (data, meta=NULL, period=NULL) {
    xref = matrix(
        c(0.099, 0.100, 0.101, 0.099, 0.088, 0.078, 0.072,
          0.064, 0.064, 0.069, 0.076, 0.089,
          0.133, 0.126, 0.111, 0.110, 0.081, 0.056, 0.038,
          0.027, 0.042, 0.063, 0.098, 0.117,
          0.128, 0.142, 0.122, 0.128, 0.105, 0.065, 0.035,
          0.024, 0.031, 0.044, 0.074, 0.101,
          0.157, 0.130, 0.119, 0.094, 0.062, 0.042, 0.028,
          0.021, 0.035, 0.062, 0.099, 0.150,
          0.204, 0.163, 0.118, 0.102, 0.060, 0.030, 0.018,
          0.012, 0.023, 0.041, 0.087, 0.143,
          0.156, 0.154, 0.117, 0.119, 0.086, 0.044, 0.025,
          0.015, 0.025, 0.044, 0.089, 0.127,
          0.139, 0.092, 0.082, 0.099, 0.087, 0.039, 0.015,
          0.012, 0.036, 0.108, 0.159, 0.131,
          0.112, 0.098, 0.101, 0.125, 0.122, 0.072, 0.036,
          0.024, 0.039, 0.067, 0.102, 0.102,
          0.058, 0.050, 0.100, 0.142, 0.158, 0.092, 0.067,
          0.050, 0.042, 0.058, 0.083, 0.100,
          0.050, 0.050, 0.058, 0.083, 0.150, 0.167, 0.117,
          0.083, 0.058, 0.058, 0.067, 0.058,
          0.033, 0.025, 0.033, 0.075, 0.167, 0.217, 0.142,
          0.092, 0.067, 0.058, 0.050, 0.042,
          0.017, 0.008, 0.017, 0.042, 0.108, 0.183, 0.200,
          0.175, 0.117, 0.067, 0.042, 0.025),
        ncol=12, byrow=TRUE)
    colnames(xref) = seq(1, 12, 1)
    row.names(xref) = c('GROUP1', 'GROUP2', 'GROUP3', 'GROUP4',
                        'GROUP5', 'GROUP6', 'GROUP7', 'GROUP8',
                        'GROUP9', 'GROUP10', 'GROUP11', 'GROUP12')  
    
    # If there is a specified period
    if (!is.null(period)) {
        # Extracts only the data of this period
        subdata = data[data$Date >= as.Date(period[1])
                          & data$Date <= as.Date(period[2]),]
    } else {
        subdata = data
    }
    
    # If there is the metadata
    if (!is.null(meta)) {
        # New column in metadata for hydrological regime
        meta$regimeHydro = NA
        meta$typologie_regimeHydro = NA
        # New column in metadata for the start of the hydrological year
        meta$maxQM = NA
        meta$minQM = NA
        
        # Get all different stations code
        Code = levels(factor(subdata$Code))
        # Number of stations
        nCode = length(Code)
        
    # Otherwise it is just a list of flow from one station
    } else {
        # Only one code is present
        nCode = 1
    }

    # Blank tibble to store data
    QM = tibble()
    # For all accessible code
    for (k in 1:nCode) {
        # If there is the metadata
        if (!is.null(meta)) {
            # Gets the code
            code = Code[k]
            # Get the associated data
            subdata_code = subdata[subdata$Code == code,]
            
        } else {
            # The data are the date for the current code
            subdata_code = subdata
        }

        # Gets a list of the month of the data as numeric
        monthData = as.numeric(format(subdata_code$Date, "%m"))
        # Blank list to stock month mean
        QM_code = c()
        # For all months
        for (i in 1:12) {
            # Gets all the flow data associated to the current month
            Q = subdata_code$Q[monthData == i]
            # Averages the data
            QM_code[i] = mean(Q, na.rm=TRUE)
        }

        regime = 0
        classRegime = ""
        distance = rep(0, length(xref[,1]))
        distancemin = 0
        for (j in 1:length(xref[,1])) {
            distance[j] = sum((QM_code / mean(QM_code) - xref[j,])^2)
        }
        regime = which.min(distance)
        distancemin = distance[which.min(distance)]
        
        if (regime < 7) {
            classRegime = "Pluvial"

        } else if (regime >= 7 & regime < 10) {
            classRegime = "Transition"
            
        } else if (regime >= 10) {
            classRegime = "Nival Glaciaire"
        } 
        
        # If there is the metadata
        if (!is.null(meta)) {
            # Creates a temporary tibble to store hydrograph results
            QMtmp = tibble(QM=QM_code, Code=code)
            # Stores it
            QM = bind_rows(QM, QMtmp)
            # Stores result of the hydrological regime
            meta$regimeHydro[meta$Code == code] = classRegime
            meta$typologie_regimeHydro[meta$Code == code] = regime
            
            # Computes the month of the max QM
            maxQM = which.max(QM_code)
            # Computes the month of the max QM
            minQM = which.min(QM_code)
            # Stores it as the start of the hydrological year
            meta$maxQM[meta$Code == code] = maxQM
            meta$minQM[meta$Code == code] = minQM
            
        # Otherwise
        } else {
            # No tibble needed
            QM = QM_code
            meta = classRegime
        }
    }
    # Returns the hydrograph and meta data
    return (list(QM=QM, meta=meta))
}


## 2. BREAK DATE _____________________________________________________
# Compute the break date of the flow data by station 
#' @title Break
#' @export
get_break = function (data, meta, level=0.1) {
    
    # Get all different stations code
    Code = rle(data$Code)$value
    # Number of stations
    nCode = length(Code)

    # Blank date break list and associated station code vector
    Date_break = list()
    Code_break = c()
    Signif_break = c()

    # For all accessible code
    for (code in Code) {
        # Get the associated data
        data_code = data[data$Code == code,] 
        # Remove NA data
        data_codeNoNA = data_code[!is.na(data_code$Q),]

        # Perform the break analysis thanks to the Pettitt test
        res_break = pettitt.test(data_codeNoNA$Q)

        # Extract p value
        p_value = res_break$p
        # The length of the data analysed
        nbreak = res_break$nobs
        # Index of the break date
        ibreak = res_break$estimate

        # Get the mean of the index break if there is several
        ibreak = round(mean(ibreak), 0)
        # Store the date break with its associated code
        Date_break = append(Date_break, 
                            data_codeNoNA$Date[ibreak])
        Code_break = append(Code_break, code)
        Signif_break = append(Signif_break, p_value <= level)

        # step1 = mean(data_codeNoNA$Q[1:ibreak])
        # step2 = mean(data_codeNoNA$Q[(ibreak+1):nbreak])
    }
    # Create a tibble with the break analysis results
    break = tibble(Code=Code_break, Date=as.Date(Date_break),
                      significant=Signif_break)
    return (break)
}


## 3. TIME GAP _______________________________________________________
# Compute the time gap by station
#' @title Time gap
#' @export
get_lacune = function (data, meta) {
    
    # Get all different stations code
    Code = rle(data$Code)$value
    
    # Create new vector to stock results for cumulative and mean
    # time gap by station
    tLac = c()
    meanLac = c()

    # Get rows where there is no NA
    NoNA = complete.cases(data)
    # Get data where there is no NA
    data_NoNA = data[NoNA,]

    # For every station
    for (code in Code) {   
        # Get only the data rows for the selected station
        data_code = data[data$Code==code,]
        # Get date for the selected station
        Date = data_code$Date
        # Get time span for the selection station
        span = as.numeric(Date[length(Date)] - Date[1])
        
        # Get only the data rows with no NA for the selected station
        data_NoNA_code = data_NoNA[data_NoNA$Code==code,]
        # Get date for the selected station
        Date_NoNA = data_NoNA_code$Date
        
        # Compute the time gap
        lac = as.numeric(diff(Date_NoNA) - 1)

        # Compute the cumulative gap
        lac_sum = sum(lac)
        # Store the cumulative gap rate
        tLac = c(tLac, lac_sum/span)

        # Compute the mean gap
        lac_mean = mean(lac[lac != 0])
        # Store the mean gap
        meanLac = c(meanLac, lac_mean) 
    }
    
    # Compute the cumulative gap rate in pourcent
    tLac100 = tLac * 100
    # Create tibble for lacune
    lac = tibble(Code=Code, tLac100=tLac100, meanLac=meanLac)
    # Join a tibble
    meta = full_join(meta, lac, by="Code")
    return (meta)
}
