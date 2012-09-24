package networkDomain.core;

import networkDomain.NetworkNode;
import networkDomain.NetworkSignal;
import dataDomain.DataCell;
import tools.concurrency.Port;
import tools.errorChecking.Assert;

public class InputProcess extends Thread {
	
	private Port<NetworkSignal> port;
	NetworkNode parent;
	
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	public InputProcess() {
		port = new Port<NetworkSignal>();
	}
	
	// note that here the sending node is ignored, but may be used if this class is extended.
	public void acceptSignal(NetworkSignal signal, NetworkNode sender) {
		port.send(signal);
	}

	@Override
	public void run() {
		NetworkSignal signalReceived;
		DataCell ProcessedData;
		
		while (true) {
			try {
				signalReceived = port.receive();
				Assert.AssertTrue("dataCell is always not null", signalReceived != null);
				
				// Process
				ProcessedData = parent.dataProcessing.processData(signalReceived.getData());
				// check that it's a different dataCell object that's returned
				Assert.AssertTrue("Data Processing returns new data cell", signalReceived.getData().getUUID() != ProcessedData.getUUID());
				
				// Store
				parent.storageProcess.storeDataCell(ProcessedData);
				
				// Add energy received (originalEnergyValue)
				// Plus modify energy level to account for energy lost or gained in processing
				parent.energyEconomics.offsetEnergy(2 * signalReceived.getData().getEnergy() - ProcessedData.getEnergy());
				
			} catch (InterruptedException e) {/*Ignore and repeat*/}
		}
	}
	
	
}
