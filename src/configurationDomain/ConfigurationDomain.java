package configurationDomain;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;

import configurationDomain.exceptions.FileAlreadyLoadedException;
import configurationDomain.exceptions.SectionAlreadyExistsException;
import configurationDomain.exceptions.SectionNotFoundException;
import configurationDomain.exceptions.SettingNotFoundException;

import application.Domain;
import application.DomainContainer;

public class ConfigurationDomain extends Domain {

	private HashMap<String, HashMap<String, String>> settings;
	private HashMap<String, String> sectionFiles;
	
	public ConfigurationDomain(DomainContainer container) {
		super(container);
	}

	@Override
	public void initialise() {
		
		settings = new HashMap<String, HashMap<String, String>>();
		sectionFiles = new HashMap<String, String>();
		
	}
	
	public String getSetting(String section, String name) throws SectionNotFoundException, SettingNotFoundException {
		HashMap<String, String> sectionMap;
		
		if (!settings.containsKey(section))
			throw new SectionNotFoundException();
		
		sectionMap = settings.get(section);
		
		if (!sectionMap.containsKey(name))
			throw new SettingNotFoundException();
		
		return settings.get(section).get(name);
	}
	
	public HashMap<String, String> getSection(String section) throws SectionNotFoundException {
		
		if (!settings.containsKey(section))
			throw new SectionNotFoundException();
		
		return settings.get(section);
	}
	
	public void setSetting(String section, String name, String value) throws SectionNotFoundException {
		
	}
	
	public void makeSettingSection(String section) throws SectionAlreadyExistsException {
		
		if (sectionFiles.containsKey(section))
			throw new SectionAlreadyExistsException();
		
		sectionFiles.put(key, value)
		
	}
	
	public void removeSettingSection(String section) throws SectionNotFoundException {
		
	}
	
	public void saveConfig(String section) {
		
	}
	
	public void loadConfig(String filename, String newSection) throws FileAlreadyLoadedException, SectionAlreadyExistsException, XMLStreamException, FileNotFoundException {
		
		if (sectionFiles.containsValue(filename))
			throw new FileAlreadyLoadedException();
		
		XMLInputFactory factory = XMLInputFactory.newInstance();
		XMLEventReader r = factory.createXMLEventReader(filename, new FileInputStream(filename));
		XMLEvent e;
		
		String section = newSection;
		String upSection = newSection;
		ArrayList<String> sectionList = new ArrayList<String>();
		
		makeSettingSection(newSection);
		
		// Make sure the document has a <configuration> tag as the main tag
		if (r.hasNext()) {
			
			try {
				e = r.nextEvent();
			} catch (XMLStreamException e1) {
				try {
					removeSettingSection(newSection);
				} catch (SectionNotFoundException e2) {}
				throw e1;
			}
			
			if (!(e.getEventType() == XMLEvent.START_DOCUMENT)) {
				System.out.println("No start document"); // TODO
				return;
			}
			
			try {
				e = r.nextEvent();
			} catch (XMLStreamException e1) {
				try {
					removeSettingSection(newSection);
				} catch (SectionNotFoundException e2) {}
				throw e1;
			}
			
			if (!(e.getEventType() == XMLEvent.START_ELEMENT && e.toString().equals("<configuration>"))) {
				System.out.println("<configuration> not the main tag: " + e.toString()); // TODO
				return;
			}
			
		}
		
		while (r.hasNext()) {
			
		    try {
				e = r.nextEvent();
			} catch (XMLStreamException e1) {
				try {
					removeSettingSection(newSection);
				} catch (SectionNotFoundException e2) {}
				throw e1;
			}
		    
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
		    		
		    		if (!settings.containsKey(upSection)) {
		    			settings.put(upSection, new HashMap<String, String>());
		    		}
		    		settings.get(upSection).put(sectionList.get(sectionList.size() - 1), e.asCharacters().getData());
		    		
		    			
		    	}
		    	break;
		    }
		}

	}
		
	// Testing
	public static void main(String[] args) {
		ConfigurationDomain c = new ConfigurationDomain(new DomainContainer());
		
		
		c.initialise();
		
		try {
			c.loadConfig("config/interface.xml", "interface");
		} catch (Exception e) {
		
		}
		
		try {
			System.out.println(c.getSetting("interface", "type"));
		} catch (Exception e) {
			
		}
	}
	
}
