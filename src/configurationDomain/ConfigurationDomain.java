package configurationDomain;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;

import application.Domain;
import application.DomainContainer;

public class ConfigurationDomain extends Domain {

	private HashMap<String, HashMap<String, String>> settings;
	
	public ConfigurationDomain(DomainContainer container) {
		super(container);
	}

	@Override
	public void initialise() {
		
		settings = new HashMap<String, HashMap<String, String>>();
		
		loadConfig();
		
	}
	
	public String getSetting(String section, String name) throws SectionNotFoundException, SettingNotFoundException {
		
		return name;
	}
	
	public HashMap<String, String> getSection(String section) throws SectionNotFoundException {
		return null;
	}
	
	public void setSetting(String section, String name, String value) throws SectionNotFoundException {
		
	}
	
	public void makeSettingSection(String section) {
		
	}
	
	public void removeSettingSection(String section) throws SectionNotFoundException {
		
	}
	
	public void saveConfig() {
		
	}
	
	public void loadConfig() {
		try {
			String filename = "config/interface.xml"; // TODO recurse entire directory
			
			XMLInputFactory factory = XMLInputFactory.newInstance();
			XMLEventReader r = factory.createXMLEventReader(filename, new FileInputStream(filename));
			XMLEvent e;
			
			String section = "interface"; //TODO
			String upSection = "interface";
			ArrayList<String> sectionList = new ArrayList<String>();
			
			// Make sure the document has a <configuration> tag as the main tag
			if (r.hasNext()) {
				e = r.nextEvent();
				if (!(e.getEventType() == XMLEvent.START_DOCUMENT)) {
					System.out.println("No start document"); // TODO
					return;
				}
				e = r.nextEvent();
				if (!(e.getEventType() == XMLEvent.START_ELEMENT && e.toString().equals("<configuration>"))) {
					System.out.println("<configuration> not the main tag: " + e.toString()); // TODO
					return;
				}
			}
			
			while (r.hasNext()) {
			    e = r.nextEvent();
			    
			    switch (e.getEventType()) {
			    case XMLEvent.START_ELEMENT:
			    	sectionList.add(e.asStartElement().getName().getLocalPart());
			    	upSection = section;
			    	section = section + "." + e.asStartElement().getName().getLocalPart();
			    	break;
			    case XMLEvent.END_ELEMENT:
			    	if (sectionList.size() > 0) {
			    		section = upSection;
			    		if (sectionList.size() > 1)
			    			upSection = upSection.substring(0, section.length() - sectionList.get(sectionList.size() - 1).length()); 
			    		sectionList.remove(sectionList.size() - 1);
			    	}
			    	break;
			    case XMLEvent.CHARACTERS:
			    	if (!(e.isCharacters() && e.asCharacters().isWhiteSpace())) {
			    		System.out.println("Characters: " + e.asCharacters().getData());
			    		
			    		//settings.put(section.substring(0, section.length() - sectionList.get(sectionList.size() - 1).length()), arg1)
			    	}
			    	break;
			    }
			    System.out.println("section: " + section + ", upSection: " + upSection);
			}
			
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (XMLStreamException e) {
			e.printStackTrace();
		}	
	}
		
	// Testing
	public static void main(String[] args) {
		new ConfigurationDomain(new DomainContainer()).initialise();
	}
	
}
