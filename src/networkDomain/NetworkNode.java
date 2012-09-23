package networkDomain;

import java.util.ArrayList;
import tools.Log;
import dataDomain.DataCell;
import mechanismsDomain.ApplicationEventQueue;
import mechanismsDomain.State;
import mechanismsDomain.Transition;
import networkDomain.extensions.NodeExtensionEncapsulator;
import networkDomain.visitors.NetworkVisitable;
import networkDomain.visitors.NetworkVisitor;
import environmentsDomain.Action;

/**
 * 
 * A NetworkNode is defined as:
 *   An active class with two states that can receive, process and send data.
 *   It contains an input vector (source node, data) pairs
 *   It possesses all network domain extensions
 * 
 * @author Loren
 *
 */
public class NetworkNode extends NetworkComponent implements NetworkVisitable {
	
	public NetworkDomain domain;
	public NetworkNodeProperties properties;
	
	public static final boolean IS_OUTPUT_NODE = true;
	public static final boolean IS_NOT_OUTPUT_NODE = false;
	
	public boolean finalised = false;
	
	public static final int ACTIVATATION_EVENT		= 0;
	public static final int DEACTIVATATION_EVENT	= 1;
	
	private boolean setToFire;
	
	protected class ActiveState extends State {
		NetworkNode associatedNode;
		public ActiveState(NetworkNode associatedNode) {
			this.associatedNode = associatedNode;
		}
		public void procedure(Object parameters) {
			NetworkSignal signalToFire;
			NetworkComponent target;
			
			// prevent properties class from processing
			properties.preventProcessing = true;
			
			// loop while FiringCondition, EnergyEconomics and TransmissionContent classes permit
			while (properties.NXE.firingCondition.continueFiring() && 
				   properties.NXE.energyEconomics.continueFiring() && 
				   properties.NXE.transmissionContent.signalsRemain()) {
				
				// allow TransmissionContent class to do what it wants with data
				signalToFire = properties.NXE.transmissionContent.nextSignalToFire(); 
				
				// allow TargetSelection class to pick a target node
				target = properties.NXE.targetSelection.selectTarget(signalToFire);
				if (target instanceof Action) {
					
					// perform action targeted
					((Action) target).performAction(signalToFire);
					Log.write("Node " + associatedNode.networkComponentUID + " performed action.");
					
				} else {
				
					// fire content to node
					((NetworkNode) target).acceptSignal(signalToFire, associatedNode);
					Log.write("Data (" + signalToFire.getData(associatedNode).getDatum().getValue() 
							+ ") passed from node " + associatedNode.networkComponentUID 
							+ " to node " + ((NetworkNode) target).networkComponentUID);
					
				}
				
			}
			
			// re-enable properties processing
			properties.preventProcessing = false;
			
			// send DEACTIVATATION_EVENT to self, immediate
			ApplicationEventQueue.generateEvent(DEACTIVATATION_EVENT, null, associatedNode);
			
		}
	}
	
	protected class InactiveState extends State {
		NetworkNode associatedNode;
		public InactiveState(NetworkNode associatedNode) {
			this.associatedNode = associatedNode;
		}
		public void procedure(Object parameters) {
			// Inform FiringCondition that node has become inactive (So that it may send a delayed fire event)
			properties.NXE.firingCondition.notifyNodeBecameInactive();
			// Allow node to be fired again
			setToFire = false;
		}
	}
	
	private void initialiseStates() {
		ActiveState activeState = new ActiveState(this);
		InactiveState inactiveState = new InactiveState(this);
		
		addState(activeState);
		addState(inactiveState);
		
		addTransition(new Transition(inactiveState, activeState, ACTIVATATION_EVENT));
		addTransition(new Transition(activeState, inactiveState, DEACTIVATATION_EVENT));
		
		setInitialState(inactiveState);
	}
	
	public NetworkNode(NetworkDomain domain, NetworkNode parent, NodeExtensionEncapsulator NXE, boolean isOutputNode) {
		
		properties = new NetworkNodeProperties();
		setToFire = false;
		
		properties.NXE = NXE;
		properties.parentNode = parent;
		properties.associatedNode = this;
		properties.isOutputNode = isOutputNode;
		
		NXE.setOnceNetworkNodeProperties(properties);
		
		initialiseStates();
		
		tools.Log.created(this.getClass());
	}
	
	public void registerActions(ArrayList<Action> newActions) {
		properties.actions.addAll(newActions);
	}
	
	public void acceptSignal(NetworkSignal signal, NetworkComponent sender) {
		properties.acceptData(signal, sender);
	}
	
	public void fire() {
		if (!setToFire)
			ApplicationEventQueue.generateEvent(ACTIVATATION_EVENT, null, this);
	}
	
	public NetworkNode getParent() {
		return properties.parentNode;
	}
	
	public ArrayList<NetworkNode> getInputChildrenNodes() {
		return properties.inputChildrenNodes;
	}
	
	public ArrayList<NetworkNode> getOutputChildrenNodes() {
		return properties.outputChildrenNodes;
	}
	
	public ArrayList<NetworkNode> getStandardChildrenNodes() {
		return properties.standardChildrenNodes;
	}
	
	
	/**
	 * Returns whether or not this is the root node of a network
	 * @return
	 */
	public boolean isHighestNetworkSet() {
		return (properties.parentNode == null) ? true : false;
	}
	
	public void acceptVisitor(NetworkVisitor networkVisitor) {
		if (properties.inputChildrenNodes.size() == 0 && properties.outputChildrenNodes.size() == 0 && properties.standardChildrenNodes.size() == 0) {
			tools.Log.write("Accepting visitor to NetworkNode");
			networkVisitor.visitNetworkNode(this, properties);
		} else {
			tools.Log.write("Passing visitor to NetworkNode");
			for (NetworkNode n : properties.inputChildrenNodes)
				n.acceptVisitor(networkVisitor);
			for (NetworkNode n : properties.outputChildrenNodes)
				n.acceptVisitor(networkVisitor);
			for (NetworkNode n : properties.standardChildrenNodes)
				n.acceptVisitor(networkVisitor);
		}
	}
}
