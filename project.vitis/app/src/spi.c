/******************************************************************************
* Copyright (C) 2017 - 2022 Xilinx, Inc.  All rights reserved.
* SPDX-License-Identifier: MIT
******************************************************************************/

/******************************************************************************
 * Engineer    : Brayan Lui Estalla Quinteros
 * Institution : Jicamarca Radio Observatory
 * project     : PS SPI and AXI SPI
 * Vitis       : 2022.2
 * source:
 * PS SPI
 * - https://github.com/Xilinx/embeddedsw/tree/master/XilinxProcessorIPLib/drivers/spips/examples
 * - https://adaptivesupport.amd.com/s/article/796622?language=en_US
 * - https://github.com/ATaylorCEngFIET/Part_217_SPI_Master_Salve/tree/master/project_1
 * AXI SPI
 * - https://github.com/Xilinx/embeddedsw/tree/master/XilinxProcessorIPLib/drivers/spi/examples
 * - https://github.com/viktor-nikolov/ILI9488-Xilinx/blob/main/pictures/Zynq_PS-GPIO_AXI-SPI_diagram.png
 * - https://forum.digilent.com/topic/20709-using-an-spi-device-with-arty-board/
******************************************************************************/

#include <stdio.h>
#include "platform.h"
#include "xil_printf.h"
#include "xspips.h"
#include "xspi.h"
#include "sleep.h"


// Ps SPI
#define SPI_DEVICE_ID		XPAR_XSPIPS_0_DEVICE_ID
static XSpiPs PsSpi;
static XSpiPs_Config *PsSpiConfig;
u16 TxBuffer_PsSpi[2];
u16 RxBuffer_PsSpi[2];


// Axi Spi
#define QSPI_DEVICE_ID		XPAR_AXI_QUAD_SPI_0_DEVICE_ID
static XSpi AxiSpi;
static XSpi_Config *AxiSpiConfig;
u16 TxBuffer_AxiSpi[2];
u16 RxBuffer_AxiSpi[2];


int initialize_PS_SPI() {
	int Status;
	PsSpiConfig = XSpiPs_LookupConfig(SPI_DEVICE_ID);
	if (PsSpiConfig == NULL) {
		print("XSpiPs_LookupConfig failed\r\n");
		return XST_FAILURE;
	}

	Status = XSpiPs_CfgInitialize(&PsSpi, PsSpiConfig,PsSpiConfig->BaseAddress);
	if (Status != XST_SUCCESS) {
		print("XSpiPs_CfgInitialize failed\r\n");
		return XST_FAILURE;
	}

	Status = XSpiPs_SelfTest(&PsSpi);
	if (Status != XST_SUCCESS) {
		print("XSpiPs_SelfTest failed\r\n");
		return XST_FAILURE;
	}

	// Set the SPI interface as Master.
	// Set Force Slave Select option: The SPI_SS_outN signal indicated by the Slave Select Control bit is forced active (driven low)
	// modo 0 (CPOL=0, CPHA=0):  ___|----|___
	Status = XSpiPs_SetOptions(&PsSpi, XSPIPS_MASTER_OPTION | XSPIPS_FORCE_SSELECT_OPTION);
	if(Status != XST_SUCCESS) {
		print("XSpiPs_SetOptions failed\r\n");
		return XST_FAILURE;
	}

	//modo 3 (CPOL=1, CPHA=1):  ---|____|---
	//Status = XSpiPs_SetOptions(&PsSpi, XSPIPS_MASTER_OPTION | XSPIPS_FORCE_SSELECT_OPTION | XSPIPS_CLK_ACTIVE_LOW_OPTION | XSPIPS_CLK_PHASE_1_OPTION);
	//if(Status != XST_SUCCESS) {
	//	print("XSpiPs_SetOptions failed\r\n");
	//	return XST_FAILURE;
	//}

	// Select Slave 0
	Status = XSpiPs_SetSlaveSelect(&PsSpi, 0); //0x00
	if(Status != XST_SUCCESS) {
		print("XSpiPs_SetSlaveSelect failed\r\n");
		return XST_FAILURE;
	}

	// Setting SCK frequency for the PS SPI:
	// ZYNQ7 Processing System default SPI clock is 166.666666 MHz
	// Using XSPIPS_CLK_PRESCALE_64 -> SCK frequency 2.60 MHz = cycle duration 384 ns
	// Using XSPIPS_CLK_PRESCALE_16 -> SCK frequency 10.42 MHz = cycle duration 96 ns
	// Using XSPIPS_CLK_PRESCALE_8  -> SCK frequency 20.83 MHz = cycle duration 48 ns
	Status = XSpiPs_SetClkPrescaler(&PsSpi, XSPIPS_CLK_PRESCALE_64);
	if(Status != XST_SUCCESS) {
		print("XSpiPs_SetClkPrescaler failed\r\n");
		return XST_FAILURE;
	}

	return 0;
} // initialize_PS_SPI


int initialize_AXI_SPI() {
	int Status;
	AxiSpiConfig = XSpi_LookupConfig(QSPI_DEVICE_ID);
	if (AxiSpiConfig == NULL) {
		print("XSpi_LookupConfig failed\r\n");
		return XST_FAILURE;
	}

	Status = XSpi_CfgInitialize(&AxiSpi, AxiSpiConfig, AxiSpiConfig->BaseAddress);
	if (Status != XST_SUCCESS) {
		print("XSpi_CfgInitialize failed\r\n");
		return XST_FAILURE;
	}

	//modo 0 (CPOL=0, CPHA=0):  ___|----|___
	//XSpi_SetOptions(&AxiSpi, XSP_MASTER_OPTION | XSP_LOOPBACK_OPTION);
	Status = XSpi_SetOptions(&AxiSpi, XSP_MASTER_OPTION);
	if (Status != XST_SUCCESS) {
		print("XSpi_SetOptions failed\r\n");
		return XST_FAILURE;
	}

	//modo 3 (CPOL=1, CPHA=1):  ---|____|---
	//XSpi_SetOptions(&AxiSpi, XSP_MASTER_OPTION | XSP_CLK_PHASE_1_OPTION | XSP_CLK_ACTIVE_LOW_OPTION);

	//AxiSpi SpiMode => XSP_STANDARD_MODE;

	// Setting SCK frequency for the AXI SPI (PL):
	// freq_zynq/Prescaler 100Mhz/16= 6.25Mhz

	Status = XSpi_Start(&AxiSpi);
	if (Status != XST_SUCCESS) {
		print("XSpi_Start failed\r\n");
		return XST_FAILURE;
	}

	XSpi_IntrGlobalDisable(&AxiSpi);
	/*Status = XSpi_IntrGlobalDisable(&AxiSpi);
	if (Status != XST_SUCCESS) {
		print("XSpi_IntrGlobalDisable failed\r\n");
		return XST_FAILURE;
	}*/

	Status = XSpi_SetSlaveSelect(&AxiSpi, 0b1);
	if (Status != XST_SUCCESS) {
		print("XSpi_SetSlaveSelect failed\r\n");
		return XST_FAILURE;
	}

	return 0;
 } //initialize_AXI_SPI


uint16_t converter(uint16_t valor) {
    return (valor >> 8) | (valor << 8);
} //converter


int main()
{
	init_platform();
	print("*** PROJECT START ***\r\n");

	// Initialize PS SPI driver
	initialize_PS_SPI();

	// Initialize AXI SPI driver
	initialize_AXI_SPI();

	// PS SPI Parameters
    int i;
    TxBuffer_PsSpi[0] = converter(0x0201); //converter
    TxBuffer_PsSpi[1] = converter(0x0703); //converter
	//Data transfer of PsSpi in oscilloscope 0102-0307 | MSB - bits(15-0,31-16)
    //Data transfer of PsSpi in oscilloscope 0201-0703 | MSB - bits(31-16,15-0) | (used converter)

	// AXI SPI Parameters
	int j;
	TxBuffer_AxiSpi[0] = (0x0201);
	TxBuffer_AxiSpi[1] = (0x0703);
	//Data transfer of AxiSpi in oscilloscope 0201-0703 | MSB - bits(31-16,15-0)


	while (1) {
		// SEND DATA OF PS SPI
		//leng 1 => 1 bytes or 8 bits | leng 4 => 4 bytes or 32 bits
		XSpiPs_PolledTransfer(&PsSpi, (u8*)&TxBuffer_PsSpi, (u8*)&RxBuffer_PsSpi, 4);
		for(i=0 ; i<2 ; i++){
			printf("RxBuffer_PsSpi : %04x \n\r", converter(RxBuffer_PsSpi[i]));
		}

		// SEND DATA OF AXI SPI
		//leng 1 => 1 bytes or 8 bits | leng 4 => 4 bytes or 32 bits
		XSpi_Transfer(&AxiSpi, (u8*)&TxBuffer_AxiSpi, (u8*)&RxBuffer_AxiSpi, 4);
		for(j=0 ; j<2 ; j++){
			printf("RxBuffer_AxiSpi : %04x \n\r", RxBuffer_AxiSpi[j]);
		}

	}

    cleanup_platform();
    return 0;
}


