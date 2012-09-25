package networkDomain.core;

import java.util.Observable;
import java.util.Observer;
import networkDomain.NetworkNode;
import networkDomain.NetworkSignal;
import networkDomain.NetworkTransmitter;
import dataDomain.DataCell;
import environmentDomain.Perception;
import tools.concurrency.Port;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;

public class InputProcess extends Thread implements Observer {
	
	private Port<NetworkSignal> port;
	NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	private class replicator extends Thread {
		@SuppressWarnings("unused") InputProcess newFunction;
		public replicator(InputProcess newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); }
	}
	public void replicateFunction(InputProcess newFunction) { (new replicator(newFunction)).start(); }
	public InputProcess replicate() {
		InputProcess n;
		try {
			n = this.getClass().newInstance();
			return n; 
		} catch (InstantiationException e) {
			e.printStackTrace();
			System.exit(1);
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			System.exit(1);
		} 
		return null;
	}
	
	public InputProcess() {
		port = new Port<NetworkSignal>();
	}
	
	// note that here the sending node is ignored, but may be used if this class is extended.
	public void acceptSignal(NetworkSignal signal, NetworkTransmitter sender) {
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
	
	// Receive perception (arg0) data (arg1)
	// Process as any other signal
	@Override
	public void update(Observable arg0, Object arg1) {
		Assert.AssertTrue("Observable is Perception", arg0 instanceof Perception);
		Assert.AssertTrue("Object is Signal", arg1 instanceof NetworkSignal);
		acceptSignal((NetworkSignal) arg1, (Perception) arg0);
		
		Log.writeForMechanisms("Received signal via observable");
	}
	
	
}
