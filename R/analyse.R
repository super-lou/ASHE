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



## 1. HYDROGRAPH _____________________________________________________
hide_find_regimeHydro = function (QM_code, forceId=NA, forceIdIf=NA,
                                  check=FALSE) {
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
    groupname = c('GROUP1', 'GROUP2', 'GROUP3', 'GROUP4',
                  'GROUP5', 'GROUP6', 'GROUP7', 'GROUP8',
                  'GROUP9', 'GROUP10', 'GROUP11', 'GROUP12')
    row.names(xref) = groupname

    if (check) {
        dev_path = file.path(dirname(dirname(dirname(getwd()))),
                             'dataSHEEP_project', 'dataSHEEP',
                             "__SHEEP__")
        if (file.exists(dev_path)) {
            list_path = list.files(dev_path,
                                   pattern='*.R$',
                                   full.names=TRUE,
                                   recursive=TRUE)
            for (path in list_path) {
                source(path, encoding='UTF-8')    
            }
        }
        library(ggplot2)
        outdir = "regimeHydro_check"
        if (!(file.exists(outdir))) {
            dir.create(outdir, recursive=TRUE)
        }
        for (i in 1:length(groupname)) {
            plot = panel_hydrograph(xref[i,], groupname[i])
            ggplot2::ggsave(plot=plot,
                            filename=paste0(groupname[i], ".pdf"),
                            path=outdir,
                            width=5, height=3,
                            units="cm",
                            dpi=100)
        }
        return ()
    }
    
    id = 0
    typology = ""
    distance = rep(0, length(xref[,1]))
    # distancemin = 0
    for (j in 1:length(xref[,1])) {
        distance[j] = sum((QM_code / mean(QM_code, na.rm=TRUE) - xref[j, ])^2)
    }

    id_tmp = which.min(distance)
    if (all(!is.na(forceId))) {
        if (all(!is.na(forceIdIf))) {
            if (id_tmp %in% forceIdIf) {
                id = which.min(distance[forceId]) + min(forceId) - 1
            } else {
                id = id_tmp
            }
        } else {
            id = which.min(distance[forceId]) + min(forceId) - 1
        }
    } else {
        id = id_tmp
    }

    id = as.numeric(id)
    # distancemin = distance[which.min(distance)]

    if (id < 7) {
        typology = "Pluvial"

    } else if (id >= 7 & id < 10) {
        typology = "Transition"
        
    } else if (id >= 10) {
        typology = "Nival Glaciaire"
    }
    
    if (id == 1) {
        detail = "Pluvial faiblement contrasté"

    } else if (id %in% 2:3) {
        detail = "Pluvial modérément contrasté"
        
    } else if (id %in% 5:6) {
        detail = "Pluvial fortement contrasté"

    } else if (id == 7) { 
        detail = "Pluvio-nival"

    } else if (id %in% 8:9) {
        detail = "Nivo-pluvial"

    } else if (id == 10) {
        detail = "Nival"
        
    } else if (id %in% 11:12) {
        detail = "Nivo-glaciaire"
    }

    regimeHydro = list(id=id,
                       typology=typology,
                       detail=detail)
    return (regimeHydro)
}


# Computes the hydrograph of a station
#' @title Hydrograph
#' @export
find_regimeHydro = function (dataEXserieQM,
                             lim_number=NULL,
                             dataEXseriePA=NULL
                             ) {
    
    if (!is.null(dataEXseriePA)) {
        isMOD =
            dplyr::summarise(dplyr::group_by(dataEXseriePA,
                                             Code),
                             isPluvial=mean(PAs, na.rm=TRUE) <
                                 0.05*mean(PA, na.rm=TRUE),
                             isSnow=mean(PAs, na.rm=TRUE) >=
                                 0.05*mean(PA, na.rm=TRUE),
                             .groups="drop")
        isMOD$forceId = NA
        isMOD$forceIdIf = NA
        isMOD$forceId[isMOD$isPluvial] = list(1:7)
        isMOD$forceId[isMOD$isSnow] = list(7:9)
        isMOD$forceIdIf[isMOD$isSnow] = list(1:3)
        dataEXserieQM = dplyr::full_join(dataEXserieQM,
                                         dplyr::select(isMOD,
                                                       c("Code",
                                                         "forceId",
                                                         "forceIdIf")),
                                         by="Code")
    } else {
        dataEXserieQM$forceId = NA
        dataEXserieQM$forceIdIf = NA
    }

    regimeHydro =
        dplyr::summarise(dplyr::group_by(dataEXserieQM,
                                         Code),
                         as_tibble(
                             hide_find_regimeHydro(QM,
                                                   forceId[[1]],
                                                   forceIdIf[[1]])),
                         .groups="drop")

    if (!is.null(lim_number)) {
        find_nearest = function (id, Id, lim_number) {
            nId = table(Id)
            nId = nId[nId >= lim_number]
            nId_value = as.numeric(names(nId))
            disp = abs(nId_value - id)
            id = max(nId_value[disp == min(disp)])
            return (id)
        }        
        regimeHydro =
            dplyr::mutate(dplyr::group_by(regimeHydro, id),
                          id=dplyr::if_else(dplyr::n() < lim_number,
                                            find_nearest(id[1],
                                                         regimeHydro$id,
                                                         lim_number),
                                            id[1]),
                          .keep="all")
    }

    regimeHydro$id = formatC(regimeHydro$id, width=2, flag="0")
    return (regimeHydro)
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
