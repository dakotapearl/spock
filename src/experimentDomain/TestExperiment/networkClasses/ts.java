package experimentDomain.TestExperiment.networkClasses;

import java.util.ArrayList;
import java.util.Random;
import dataDomain.DataCell;
import networkDomain.NetworkNode;
import networkDomain.NetworkSignal;
import networkDomain.extensions.NodeExtensionEncapsulator;
import networkDomain.extensions.TargetSelection;

public class ts implements TargetSelection {

	NodeExtensionEncapsulator NXE;
	
	@Override
	public void setNXE(NodeExtensionEncapsulator NXE) {
		this.NXE = NXE;
	}

	@Override
	public NetworkNode selectTarget(NetworkSignal signal) {
		//tools.Log.write("Test Extension: TargetingSelection received selectTarget command");
		
		NetworkNodeProperties p = NXE.getNetworkNodeProperties();
		
		ArrayList<NetworkNode> accessibleNodes = new ArrayList<NetworkNode>();
		
		accessibleNodes.addAll(p.inputChildrenNodes);
		accessibleNodes.addAll(p.outputChildrenNodes);
		accessibleNodes.addAll(p.standardChildrenNodes);
		
		accessibleNodes.addAll(p.actions);
		
		if (p.parentNode != null) {
			accessibleNodes.addAll(p.parentNode.getInputChildrenNodes());
			accessibleNodes.addAll(p.parentNode.getOutputChildrenNodes());
			accessibleNodes.addAll(p.parentNode.getStandardChildrenNodes());
			
			if (p.isOutputNode)
				accessibleNodes.add(p.parentNode);
		}
		
		int targetIndex = (int) (new Random()).nextInt(accessibleNodes.size());
		
		NetworkNode target = accessibleNodes.get(targetIndex);
		
		return target;
	}

}
