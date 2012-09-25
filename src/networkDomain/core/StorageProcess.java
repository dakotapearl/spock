package networkDomain.core;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;

import dataDomain.DataCell;
import environmentDomain.Action;
import networkDomain.NetworkNode;

public class StorageProcess {
	
	// Storage of data, continuous signals, and other nodes that are connected to this one, including actions
	NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }

	private class replicator extends Thread {
		@SuppressWarnings("unused") StorageProcess newFunction;
		public replicator(StorageProcess newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); }
	}
	public void replicateFunction(StorageProcess newFunction) { (new replicator(newFunction)).start(); }
	public StorageProcess replicate() {
		StorageProcess n;
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
	
	protected ArrayList<Action> actions;
	protected Queue<DataCell> data;
	
	public StorageProcess() {
		//TODO init
		actions = new ArrayList<Action>();
		data = new LinkedList<DataCell>();
	}
	
	public void storeDataCell(DataCell dataCell) {
		synchronized (this) {
			data.add(dataCell);
			notifyAll();
		}
		parent.firingCondition.refresh(); // Not sure about this
	}
	
	public DataCell retrieveDataCell() throws InterruptedException {
		synchronized (this) {
			while (data.size() == 0) wait();
			
			return data.remove();
		}
	}
	
	public boolean hasData() {
		return data.size() > 0;
	}
	
	public void registerAction(Action action) {
		synchronized (actions) {
			actions.add(action);
		}
	}
	
	public Action getAction(int index) {
		return actions.get(index);
	}
	
}
