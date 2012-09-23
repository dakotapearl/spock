package environmentsDomain;

import java.util.ArrayList;
import dataDomain.DataDomain;
import mechanismsDomain.State;
import mechanismsDomain.Transition;
import networkDomain.NetworkComponent;
import networkDomain.NetworkNode;
import networkDomain.NetworkSignal;

public abstract class Perception extends NetworkComponent {

	protected DataDomain dataDomain;
	protected ArrayList<NetworkNode> sensors;
	
	public static final int MAKE_ACCESSIBLE_EVENT		= 0;
	public static final int MAKE_INACCESSIBLE_EVENT		= 1;
	
	protected abstract void accessibilityFunction();
	protected abstract void inaccessibilityFunction();
	
	protected class PerceptionAccessible extends State {
		public void procedure(Object parameters) {
			accessibilityFunction();
		}
	}
	
	protected class PerceptionInaccessible extends State {
		public void procedure(Object parameters) {
			inaccessibilityFunction();
		}
	}
	
	public Perception(DataDomain dataDomain) {
		this.dataDomain = dataDomain;
		sensors = new ArrayList<NetworkNode>();
		
		PerceptionAccessible perceptionAccessible = new PerceptionAccessible();
		PerceptionInaccessible perceptionInaccessible = new PerceptionInaccessible();
		
		addState(perceptionAccessible);
		addState(perceptionInaccessible);
		
		addTransition(new Transition(perceptionAccessible, perceptionInaccessible, MAKE_INACCESSIBLE_EVENT));
		addTransition(new Transition(perceptionInaccessible, perceptionAccessible, MAKE_ACCESSIBLE_EVENT));
		
		setInitialState(perceptionInaccessible);
		
		tools.Log.created(this.getClass());
	}
	
	public void registerSensors(ArrayList<NetworkNode> newSensor) {
		sensors.addAll(newSensor);
	}
	
	protected void sendSignalToSensors(NetworkSignal signal) {
		for (NetworkNode s : sensors)
			s.acceptSignal(signal, (NetworkComponent) this);
	}
	
	public abstract boolean isInitiallyAccessible();
	
}
