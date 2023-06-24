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


## 1. EXTREMES OF VALUE FOR ALL STATION ______________________________
#' @title Extremes
#' @export
get_valueExtremes = function (dataEX, trendEX,
                              mean_period=NULL,
                              colorForce=FALSE,
                              minProb=0, maxProb=1) {

    Code = levels(facotr(dataEX$Code))
    nCode = length(Code)
    Period = unique(trendEX$period)
    nPeriod = length(Period)
    Var =  levels(factor(trendEX$var))
    nVar = length(Var)
    
    X_code = array(rep(1, nPeriod*nVar*nCode),
                   dim=c(nPeriod, nVar, nCode))

    if (!is.null(mean_period)) {
        dataMeantmp = array(rep(NA, nVar*nCode),
                            dim=c(nVar, nCode))
    }

    for (j in 1:nPeriod) {
        period = Period[[j]]

        for (k in 1:nCode) {
            code = Code[k]
            
            for (i in 1:nVar) {
                var = Var[i]
                unit = metaEX$unit

                if (!is.null(mean_period)) {
                    Start = mean_period[[j]][1]
                    End = mean_period[[j]][2]
                    
                } else {
                    trendEX_period_code_var =
                        trendEX[sapply(lapply(trendEX$period,
                                              '==', period), all) &
                                trendEX$Code == code &
                                trendEX$var == var,]
                    Start = period[1]
                    End = period[2]
                }
                
                # Extracts the corresponding data for the period
                dataEX_period_code = dataEX[dataEX$Date >= Start &
                                            dataEX$Date <= End &
                                            dataEX$Code == code,]

                if (!is.null(mean_period)) {
                    # Min max for the sub period
                    Datemin = min(dataEX_period_code$Date, na.rm=TRUE)
                    Datemax = max(dataEX_period_code$Date, na.rm=TRUE)

                    # Mean of the flow over the sub period
                    dataMean = mean(dataEX_period_code[[var]],
                                    na.rm=TRUE)

                    # If this in not the first period
                    if (j > 1) {
                        # Compute the difference of mean
                        Break = dataMean - dataMeantmp[i, k]
                        # Otherwise for the first period
                    } else {
                        # Stocks NA
                        Break = NA
                    }

                    # If it is a flow variable
                    if (unit == 'hm^{3}' | unit == 'm^{3}.s^{-1}') {
                        # Normalises the break by the mean of the
                        # initial period
                        value = Break / dataMeantmp[i, k]
                        # If it is a date variable
                    } else if (unit == "jour" | unit == "jour de l'année" | unit == 'jour.an^{-1}') {
                        # Just stocks the break value
                        value = Break
                    }
                    
                    # Stores the result
                    X_code[j, i, k] = value
                    # Stores temporarily the mean of the current period
                    dataMeantmp[i, k] = dataMean

                } else {
                    # Computes the number of trend analysis selected
                    Ntrend = nrow(trendEX_period_code_var)
                    # If there is more than one trend on the same period
                    if (Ntrend > 1) {
                        # Takes only the first because they are similar
                        trendEX_period_code_var = trendEX_period_code_var[1,]
                    }
                    
                    # If it is a flow variable
                    if (unit == 'hm^{3}' | unit == 'm^{3}.s^{-1}') {
                        # Computes the mean of the data on the period
                        dataMean = mean(dataEX_period_code[[var]], na.rm=TRUE)
                        # Normalises the trend value by the mean of the data
                        value = trendEX_period_code_var$a / dataMean
                        # If it is a date variable
                    } else if (unit == "jour" | unit == "jour de l'année" | unit == 'jour.an^{-1}') {
                        value = trendEX_period_code_var$a
                    }

                    # If the p value is under the threshold
                    if (trendEX_period_code_var$p <= trendEX_period_code_varlevel |
                        colorForce) {
                        # Stores the mean trend
                        X_code[j, i, k] = value
                        # Otherwise
                    } else {
                        # Do not stocks it
                        X_code[j, i, k] = NA
                    }
                }
            }
        }
    }

    # Computes the min and the max of the averaged trend for
    # all the station
    minX = apply(X_code, c(1, 2),
                 quantile, probs=minProb, na.rm=TRUE)
    maxX = apply(X_code, c(1, 2),
                 quantile, probs=maxProb, na.rm=TRUE)
    res = list(value=X_code, min=minX, max=maxX)
    return (res)
}
    

get_Nspace = function (data_code, unit, lim_pct, NspaceMax=NULL) {
    
    # If variable unit is date 
    if (unit == "jour de l'année") {
        # The number of digit is 6 because months are display
        # with 3 characters
        Nspace = 6
        if (!is.null(NspaceMax)) {
            accuracy = NULL
        }
        
    # If it is a flow variable
    } else if (unit == 'hm^{3}' | unit == 'm^{3}.s^{-1}' | unit == 'm^{3/2}.s^{-1/2}' | unit == 'jour' | unit == 'jour.an^{-1}') {
        # Gets the max number of digit on the label
        maxtmp = max(data_code$X, na.rm=TRUE)

        # If the max is greater than 10
        if (get_power(maxtmp) >= 4) {
            Nspace = 12
            if (!is.null(NspaceMax)) {
                accuracy = NULL
            }
            
        } else if (maxtmp >= 10) {
            # The number of digit is the magnitude plus
            # the first number times 2
            Nspace = (get_power(maxtmp) + 1)*2
            # Plus spaces between thousands hence every 8 digits
            Nspace = Nspace + as.integer(Nspace/8)
            if (!is.null(NspaceMax)) {
                # The accuracy is 1
                accuracy = 1
            }

        # If the max is less than 10 and greater than 1
        } else if (maxtmp < 10 & maxtmp >= 1) {
            # The number of digit is the magnitude plus
            # the first number times 2 plus 1 for the dot
            # and 2 for the first decimal
            Nspace = (get_power(maxtmp) + 1)*2 + 3
            if (!is.null(NspaceMax)) {
                # The accuracy is 0.1
                accuracy = 0.1
            }
            
        # If the max is less than 1 (and obviously more than 0)
        } else if (maxtmp < 1) {
            # Fixes the number of significant decimals to 3
            maxtmp = signif(maxtmp, 3)
            # The number of digit is the number of character
            # of the max times 2 minus 1 for the dots that
            # count just 1 space
            Nspace = nchar(as.character(maxtmp))*2 - 3
            if (!is.null(NspaceMax)) {
                # Computes the accuracy
                accuracy = 10^(-nchar(as.character(maxtmp))+3)
            }
        }
        if (unit == 'm^{3/2}.s^{-1/2}') {
            Nspace = Nspace + 1
        }
    }
    
    if (!is.null(NspaceMax)) {
        # Gets the associated number of white space
        prefix = strrep(' ', times=NspaceMax - Nspace)
        res = list(Nspace=Nspace, prefix=prefix, accuracy=accuracy)
        return (res)
        
    } else {
        return (Nspace)
    }
}


post = function(x, ...) {
    if (verbose) {
        if (MPI != "") {
            print(paste0(formatC(as.character(rank),
                                 width=3, flag=" "),
                         "/", size-1, " > ", x), ...)
        } else {
            print(x, ...)
        }
    }
}
