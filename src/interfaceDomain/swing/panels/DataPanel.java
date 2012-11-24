package interfaceDomain.swing.panels;

import interfaceDomain.swing.SwingInterface;

import java.awt.Panel;

import javax.swing.JLabel;

public class DataPanel extends Panel {

	private static final long serialVersionUID = 3512796480831118614L;
	public JLabel title;
	SwingInterface swingInterface;
	
	public DataPanel(SwingInterface swingInterface) {
		this.swingInterface = swingInterface;
		
	}
}
