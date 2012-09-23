package experimentsDomain.test.extensions;

import java.util.ArrayList;
import java.util.Random;
import dataDomain.DataCell;
import networkDomain.NetworkComponent;
import networkDomain.NetworkNodeProperties;
import networkDomain.extensions.NodeExtensionEncapsulator;
import networkDomain.extensions.TargetSelection;

public class ts implements TargetSelection {

	NodeExtensionEncapsulator NXE;
	
	@Override
	public void setNXE(NodeExtensionEncapsulator NXE) {
		this.NXE = NXE;
	}

	@Override
	public NetworkComponent selectTarget(DataCell dataCell) {
		//tools.Log.write("Test Extension: TargetingSelection received selectTarget command");
		
		NetworkNodeProperties p = NXE.getNetworkNodeProperties();
		
		ArrayList<NetworkComponent> accessibleNodes = new ArrayList<NetworkComponent>();
		
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
		
		NetworkComponent target = accessibleNodes.get(targetIndex);
		
		return target;
	}

}
