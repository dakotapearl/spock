package interfaceDomain.swing.panels;

import interfaceDomain.swing.SwingInterface;

import java.awt.Panel;

import javax.swing.JLabel;

public class MetricPanel extends Panel {

	private static final long serialVersionUID = 7060529336586817362L;
	public JLabel title;
	SwingInterface swingInterface;
	
	public MetricPanel(SwingInterface swingInterface) {
		this.swingInterface = swingInterface;
		
	}
	
}
