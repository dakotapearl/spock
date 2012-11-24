package interfaceDomain.swing.panels;

import interfaceDomain.swing.SwingInterface;

import java.awt.GridLayout;
import java.awt.Panel;

import javax.swing.JLabel;

public class NetworkPanel extends Panel {

	private static final long serialVersionUID = -7119551510698657851L;
	public JLabel title;
	public JLabel nodes,activenodes,establishedpathways;
	SwingInterface swingInterface;
	
	public NetworkPanel(SwingInterface swingInterface) {
		this.swingInterface = swingInterface;
		
		setLayout(new GridLayout(5, 1));
		
		title = new JLabel("Network             ");
		add(title);
		nodes = new JLabel("Nodes: 0");
		add(nodes);
		activenodes = new JLabel("Node activations: 0");
		add(activenodes);
		establishedpathways = new JLabel("Established pathways: 0");
		add(establishedpathways);
		
	}
}
