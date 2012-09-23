package environmentDomain;

import java.util.ArrayList;
import dataDomain.DataDomain;
import networkDomain.NetworkNode;
import networkDomain.NetworkSignal;

public abstract class Perception {

	protected DataDomain dataDomain;
	protected ArrayList<NetworkNode> sensors;
	
	public Perception(DataDomain dataDomain) {
		this.dataDomain = dataDomain;
		sensors = new ArrayList<NetworkNode>();
		
		tools.Log.created(this.getClass());
	}
	
	public void registerNodes(ArrayList<NetworkNode> newSensor) {
		sensors.addAll(newSensor);
	}
	
	public void registerNode(NetworkNode newSensor) {
		sensors.add(newSensor);
	}
	
	protected void sendSignalToSensors(NetworkSignal signal) {
		for (NetworkNode s : sensors);
			//s.acceptSignal(signal, (NetworkNode) this);
	}
	
	public abstract boolean isInitiallyAccessible();
	
}
